;;;; messages.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defun expect-property (object name type)
  (let* ((cell  (or (assoc name object :test #'eq)
                    (error "~@<Object ~S is missing property ~S.~@:>"
                           object name)))
         (value (cdr cell)))
    (unless (typep value type)
      (error "~@<Value ~S of property ~S in object ~S is of type ~S, ~
              not ~S.~@:>"
             value name object (type-of value) type))
    value))

(defun maybe-property (object name type)
  (when-let*  ((cell  (or (assoc name object :test #'eq)
                          (when nil
                            (error "~@<Object ~S is missing property ~S.~@:>"
                                   object name))))
               (value (cdr cell)))
    (unless (typep value type)
      (error "~@<Value ~S of property ~S in object ~S is of type ~S, ~
              not ~S.~@:>"
             value name object (type-of value) type))
    value))

;;; Position type

(defun parse-position* (object)
  (make-instance 'text.source-location::line+column-position
                 :line   (expect-property object :line      'non-negative-integer)
                 :column (expect-property object :character 'non-negative-integer)))

(defun unparse-position (position)
  `((:line      . ,(text.source-location:line   position))
    (:character . ,(text.source-location:column position))))

;;; Range type

(defun parse-range* (object)
  (text.source-location:make-range
   (parse-position* (expect-property object :start 'cons))
   (parse-position* (expect-property object :end   'cons))))

(defun unparse-range (range)
  `((:start . ,(unparse-position (text.source-location:start range)))
    (:end   . ,(unparse-position (text.source-location:end range)))))

;;; Location type

(defun parse-location* (object)
  (make-instance 'text.source-location:location
                 :source (make-instance 'text.source-location:source
                                        :name    (expect-property object :uri 'string)
                                        :content nil)
                 :range  (parse-range*  (expect-property object :range 'cons))))

(defun unparse-location (location)
  `((:uri   . ,(text.source-location:name (text.source-location:source location)))
    (:range . ,(unparse-range (text.source-location:range location)))))

;;; Old

(defun parse-position (thing)
  (values (the non-negative-integer (assoc-value thing :line))
          (the non-negative-integer (assoc-value thing :character))))

(defun parse-range (thing)
  (multiple-value-call #'values
    (parse-position (assoc-value thing :start))
    (parse-position (assoc-value thing :end))))

(defun parse-location (thing)
  (multiple-value-call #'values
    (values (assoc-value thing :uri))
    (parse-range (assoc-value thing :range))))

(defun parse-text-document (thing)
  (values (expect-property thing :uri     'string)
          (maybe-property  thing :version 'non-negative-integer)))

(defun parse-text-document-content-change (thing)
  (list (expect-property thing :text 'string)
        (when-let ((range (assoc-value thing :range)))
          (parse-range* range))
        (when-let ((range-length (assoc-value thing :range--length)))
          (parse-integer range-length))))
