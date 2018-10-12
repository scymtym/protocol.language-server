;;;; util.lisp --- Utilities used in the protocol module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

;;; Enums

(defmacro define-enum (name-and-options &body names-and-values)
  (let+ (((name &key (test nil test-supplied?))
          (ensure-list name-and-options))
         (names        (map 'list #'first names-and-values))
         (values       (map 'list #'second names-and-values))
         (parse-name   (symbolicate '#:parse- name))
         (unparse-name (symbolicate '#:unparse- name))
         (test         (cond (test-supplied?          test)
                             ((some #'stringp values) 'string=)))
         (value-type   (if test t `(member ,@values))))
    `(progn
       (deftype ,name ()
         '(member ,@names))

       (defun ,parse-name (value)
         (,@(if test
                `(switch (value :test ,test))
                `(case value))
           ,@(map 'list #'reverse names-and-values)
           (t (error "Invalid ~S value: ~S" ',name value))))

       (declaim (ftype (function (,name) (values ,value-type &optional))
                       ,unparse-name))
       (defun ,unparse-name (name)
         (ecase name
           ,@names-and-values)))))

;;; Message classes

(defmacro define-message-class (name (&rest lambda-list) (&rest slots))
  (let+ ((make-name    (symbolicate '#:make- name))
         (unparse-name (symbolicate '#:unparse- name))
         ((&flet+ expand ((name &rest args &key
                                (initarg (make-keyword name))
                                (type    't)
                                (reader  name)
                                &allow-other-keys))
            `(,name :initarg ,initarg :type ,type :reader ,reader
                    ,@(remove-from-plist args :initarg :type :reader))))
         (slots (map 'list (compose #'expand #'ensure-list) slots))
         ((&flet+ slot->slot ((name &rest args &key type &allow-other-keys))
            (let ((type (typecase type
                          ((cons (eql list-of))
                           `(or null (cons ,(second type))))
                          (t
                           type))))
              `(,name :type ,type ,@(remove-from-plist args :type)))))
         ((&flet+ slot->default-initarg
              ((&ign &key
                     initarg
                     ((:initform &ign) nil initform-supplied?)
                     &allow-other-keys))
            (unless initform-supplied?
              `(,initarg (error "~@<Initarg ~S is mandatory for ~S but ~
                                 has not been supplied~@:>"
                                ,initarg ',name))))) ; TODO one function
         ((&flet+ slot->initarg ((name &key initarg &allow-other-keys))
            (list initarg name)))
         ((&flet+ slot->parse ((name &key initarg type &allow-other-keys))
            (let+ ((property (make-keyword name))
                   ((&flet parser (type)
                      (typecase type
                        ((member string) nil)
                        (t               `(rcurry #'parse ',type)))))
                   ((&flet parse-value (value-form)
                      (cond
                        ((typep type '(cons (eql list-of)))
                         (if-let ((parser (parser (second type))))
                           `(map 'list ,parser ,value-form)
                           `(coerce ,value-form 'list)))
                        ((eq type 'boolean)
                         `(if ,value-form t +false+))
                        ((or (eq type t)
                             (subtypep type '(or boolean integer float string)))
                         value-form)
                        (t
                         `(funcall ,(parser type) ,value-form))))))
              (list initarg (parse-value `(expect-property data ,property ',type))))))
         ((&flet+ slot->unparse ((name &key type reader &allow-other-keys))
            (let+ ((keyword (make-keyword name))
                   ((&flet unparser (type)
                      (typecase type
                        ((member string) nil)
                        (t               (symbolicate '#:unparse- type)))))
                   ((&flet unparse-value (type value-form)
                      (cond
                        ((typep type '(cons (eql list-of)))
                         (if-let ((unparser (unparser (second type))))
                           `(map 'vector #',unparser ,value-form)
                           `(coerce ,value-form 'vector)))
                        ((eq type 'boolean)
                         `(if ,value-form t +false+))
                        ((or (eq type t)
                             (subtypep type '(or boolean integer float string)))
                         value-form)
                        (t
                         `(,(unparser type) ,value-form)))))
                   ((&labels make-cell (type &optional (value-form `(,reader message)))
                      (if (typep type '(cons (eql or) (cons (eql null))))
                          `(when-let ((value ,value-form))
                             ,(make-cell (third type) 'value))
                          `(list (cons ,keyword ,(unparse-value type value-form)))))))
              (make-cell type)))))
    `(progn
       (defclass ,name (print-items:print-items-mixin)
         ,(map 'list #'slot->slot slots)
         (:default-initargs
          ,@(mappend #'slot->default-initarg slots)))

       (defun ,make-name ,lambda-list
         (make-instance ',name ,@(mappend #'slot->initarg slots)))

       (defun ,unparse-name (message)
         (append ,@(map 'list #'slot->unparse slots)))

       (defmethod parse ((data t) (message-class (eql ',name)))
         (make-instance ',name ,@(mappend #'slot->parse slots)))

       (defmethod unparse ((message ,name))
         (append ,@(map 'list #'slot->unparse slots))))))

;;; Parsing utilities

(defun maybe-property (object name type)
  (when-let*  ((cell  (assoc name object :test #'eq))
               (value (cdr cell)))
    (unless (typep value type)
      (error "~@<Value ~S of property ~S in object ~S is of type ~S, ~
              not ~S.~@:>"
             value name object (type-of value) type))
    (values value t)))

(defun expect-property (object name type)
  (let+ (((&values value value?) (maybe-property object name type)))
    (unless value?
      (error "~@<Object ~S is missing property ~S.~@:>"
             object name))
    value))
