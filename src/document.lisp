;;;; document.lisp --- Versioned text document.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

;;; `workspace-member-mixin'

(defclass workspace-member-mixin ()
  ((%workspace :initarg  :workspace
               :accessor workspace
               :initform nil)))

(defmethod note-adopted progn ((workspace t) (document workspace-member-mixin))
  (setf (workspace document) workspace))

;;; `document'

(defclass document (workspace-member-mixin
                    print-items:print-items-mixin)
  ((%language :initarg  :language
              :type     keyword
              :reader   language)
   (%version  :initarg  :version
              :type     non-negative-integer
              :reader   version
              :accessor %version)
   (%text     :type     string
              :reader   text
              :writer   (setf %text)
              :documentation
              "Stores the current text of the document.")
   (%newlines :type     vector
              :reader   newlines
              :initform (make-array 100 :fill-pointer 0 :adjustable t)
              :documentation
              "Stores positions of newlines as indices into the string
               stored in the `text' slot."))
  (:default-initargs
   :language (error "missing required initarg :language")
   :version  (error "missing required initarg :version")))

(macrolet
    ((define-method (name)
       `(defmethod ,name :around ((instance document)
                                  &key
                                  (text nil text-supplied?))
          (call-next-method)
          (when text-supplied?
            (setf (text instance) text)))))
  (define-method initialize-instance)
  (define-method reinitialize-instance))

(defmethod print-items:print-items append ((object document))
  `((:language   ,(language object)          "~A")
    (:line-count ,(length (newlines object)) " ~D line~:P" ((:after :language)))
    (:version    ,(version object)           " @~D"        ((:after :line-count)))))

(defmethod (setf %text) :after ((new-value string)
                                (document  document))
  (let+ (((&accessors-r/o text newlines) document))
    (setf (fill-pointer newlines) 0)
    (unless (emptyp text)
      (loop :for previous = -1 :then next
            :for next = (position #\Newline text :start (1+ previous))
            :while next
            :do (vector-push-extend next newlines)))))

(defmethod (setf text) ((new-value string)
                        (document  document))
  (setf (%text document) new-value))

(defmethod update ((document document)
                   (range    text.source-location:range)
                   (new-text string))

  (let ((text (text document)))
    (text.source-location::attach-text range text)
    (let ((start-index (text.source-location:index
                        (text.source-location:start range)))
          (end-index   (text.source-location:index
                        (text.source-location:end range))))
      (log:info "updated" start-index end-index new-text)
      ;; TODO do this destructively?
      (setf (text document)
            (concatenate 'string
                         (subseq text 0 start-index)
                         new-text
                         (subseq text end-index))))))

(defmethod update ((document document)
                   (range    null)
                   (new-text string))
  (setf (text document) new-text))

(defmethod position->index ((document  document)
                            (line      integer)
                            (character integer))
  (+ (if (plusp line)
         (1+ (aref (newlines document) (1- line)))
         0)
     character))

(defmethod index->position ((document  document)
                            (index     integer))
  (let+ (((&accessors-r/o newlines) document)
         (line          (position index newlines :test #'<))
         (newline-index (if line
                            (aref newlines line)
                            0)))
    (values (1+ line) (- index newline-index 1))))

;;; Utilities

(defmethod word-at ((document document)
                    (position integer))
  (let+ (((&flet not-word? (character)
            (not (or (member character '(#\. #\? #\-) :test #'char=)
                     (alphanumericp character)))))
         (text  (text document))
         (start (1+ (or (position-if #'not-word? text
                                     :end position :from-end t)
                        -1)))
         (end   (or (position-if #'not-word? text :start position)
                    (length text))))
    (unless (= start end) ; TODO
      (subseq text start end))))

(defmethod word-at ((document document)
                    (position cons))
  (let+ (((line . character) position))
    (word-at document (position->index document line character))))
