;;;; util.lisp --- Utilities used in the protocol module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

;;; Enums

(defmacro define-enum (name-and-options &body names-and-values)
  (let+ (((name &key test) (ensure-list name-and-options))
         (names        (map 'list #'first names-and-values))
         (values       (map 'list #'second names-and-values))
         (parse-name   (symbolicate '#:parse- name))
         (unparse-name (symbolicate '#:unparse- name)))
    `(progn
       (deftype ,name ()
         '(member ,@names))

       (defun ,parse-name (value)
         (,@(if test
                `(switch (value :test ,test))
                `(case value))
           ,@(map 'list #'reverse names-and-values)
           (t (error "Invalid ~S value: ~S" ',name value))))

       (declaim (ftype (function (,name)
                                 ,@(unless test
                                     `((values (member ,@values) &optional))))
                       ,unparse-name))
       (defun ,unparse-name (name)
         (ecase name
           ,@names-and-values)))))

;;; Message classes

(defmacro define-message-class (name (&rest lambda-list) (&rest slots))
  (let+ ((make-name    (symbolicate '#:name- name))
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
         ((&flet+ slot->unparse ((name &key type reader &allow-other-keys))
            (let+ ((keyword (make-keyword name))
                   ((&flet unparser (type)
                      (typecase type
                        ((member string) nil)
                        (t               (symbolicate '#:unparse- type)))))
                   ((&flet unparse-value (value-form)
                      (typecase type
                        ((member t string)
                         value-form)
                        ((cons (eql list-of))
                         (if-let ((unparser (unparser (second type))))
                           `(map 'vector #',unparser ,value-form)
                           `(coerce 'vector ,value-form)))
                        (t
                         `(,(unparser type) ,value-form))))))
              `(list (cons ,keyword ,(unparse-value `(,reader object))))))))
    `(progn
       (defclass ,name ()
         ,(map 'list #'slot->slot slots)
         (:default-initargs
          ,@(mappend #'slot->default-initarg slots)))

       (defun ,make-name ,lambda-list
         (make-instance ',name ,@(mappend #'slot->initarg slots)))

       (defun ,unparse-name (object)
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
