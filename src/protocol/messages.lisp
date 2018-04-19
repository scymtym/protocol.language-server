;;;; messages.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

;; TODO make parse and unparse generic functions

;;; Position type

(defun parse-position (object)
  (make-instance 'text.source-location::line+column-position
                 :line   (expect-property object :line      'non-negative-integer)
                 :column (expect-property object :character 'non-negative-integer)))

(defun unparse-position (position)
  `((:line      . ,(text.source-location:line   position))
    (:character . ,(text.source-location:column position))))

;;; Range type

(defun parse-range (object)
  (text.source-location:make-range
   (parse-position (expect-property object :start 'cons))
   (parse-position (expect-property object :end   'cons))))

(defun unparse-range (range)
  `((:start . ,(unparse-position (text.source-location:start range)))
    (:end   . ,(unparse-position (text.source-location:end range)))))

;;; Location type

(defun parse-location (object)
  (let ((uri   (expect-property object :uri   'string))
        (range (expect-property object :range 'cons)))
    (make-instance 'text.source-location:location
                   :source (text.source-location:make-source uri)
                   :range  (parse-range range))))

(defun unparse-location (location)
  `((:uri   . ,(text.source-location:name
                (text.source-location:source location)))
    (:range . ,(unparse-range (text.source-location:range location)))))

;;; Text document
;;; TODO rename to versioned-text-document-identifier?

(defun parse-text-document (thing)
  (values (expect-property thing :uri     'string)
          (maybe-property  thing :version 'non-negative-integer)))

(defun unparse-text-document (uri version)
  `((:uri     . ,uri)
    (:version . ,version)))

;;; Content change

(defun parse-text-document-content-change (thing)
  (list (expect-property thing :text 'string)
        (when-let ((range (maybe-property thing :range 'cons)))
          (parse-range range))
        (when-let ((range-length (maybe-property thing :range--length 'string))) ; TODO why string?
          (parse-integer range-length))))

;;; Edit

(define-message-class edit (range new-text)
  ((range    :type text.source-location:range)
   (new-text :type string)))

;;; Text document edit

(define-message-class text-document-edit (document &rest edits)
  ((document :type string ; text-document
             )
   (edits    :type (list-of edit))))

;;; Markup content

(define-enum (markup-kind :test #'string=)
  (:plaintext "plaintext")
  (:markdown  "markdown"))

(define-message-class markup-content (value &optional (kind :plaintext))
  ((kind  :type     markup-kind
          :initform :plaintext)
   (value :type     string)))

;;; Completion

(define-enum completion-item-kind
  (:text            1)
  (:method          2)
  (:function        3)
  (:constructor     4)
  (:field           5)
  (:variable        6)
  (:class           7)
  (:interface       8)
  (:module          9)
  (:property       10)
  (:unit           11)
  (:value          12)
  (:enum           13)
  (:keyword        14)
  (:snippet        15)
  (:color          16)
  (:file           17)
  (:reference      18)
  (:folder         19)
  (:enum-member    20)
  (:constant       21)
  (:struct         22)
  (:event          23)
  (:operator       24)
  (:type-parameter 25))

(define-enum insert-text-format
  (:plain-text 1)
  (:snippet    2))

(defclass completion-item ()
  (;; Presentation
   (label         :initarg  :label
                  :reader   label)
   (kind          :initarg  :kind
                  :type     (or null completion-item-kind)
                  :reader   kind
                  :initform nil)
   (filter-text   :initarg  :filter-text
                  :type     (or null string)
                  :reader   filter-text
                  :initform nil)
   (detail        :initarg  :detail
                  :type     (or null string)
                  :reader   detail
                  :initform nil)
   (documentation :initarg  :documentation
                  :type     (or null string)
                  :reader   documentation*
                  :initform nil)
   ;; Replacement text
   (edit          :initarg  :edit
                  :reader   edit
                  :accessor %edit)
   (text-format   :initarg  :text-format
                  :type     (or null insert-text-format)
                  :reader   text-format
                  :initform :plain-text))
  (:default-initargs
   :label (error "missing required initarg")
   :edit  (error "missing required edit")))

(defun make-completion-item (label &rest args
                                   &key kind filter-text detail documentation
                                        range (new-text label) text-format)
  (declare (ignore kind filter-text detail documentation text-format))
  (apply #'make-instance 'completion-item
         :label label
         :edit  (if range
                    (make-edit range new-text)
                    new-text)
         (remove-from-plist args :range :new-text)))

(defun unparse-completion-item (item)
  `(;; Presentation
    (:label . ,(label item))
    ,@(when-let ((kind (kind item)))
        `((:kind . ,(unparse-completion-item-kind kind))))
    ,@(when-let ((filter-text (filter-text item)))
        `((:filter-text . ,filter-text)))
    ,@(when-let ((detail (detail item)))
        `((:detail . ,detail)))
    ,@(when-let ((documentation (documentation* item)))
        `((:documentation . ,documentation)))
    ;; Replacement text
    (:text-edit . ,(unparse-edit (edit item)))
    ,@(let ((text-format (text-format item)))
        (unless (eq text-format :plain-text)
          `((:insert-text-format . ,(unparse-insert-text-format text-format)))))))

;;; Hover result

(defclass hover-result ()
  ((content :initarg  :content
            :type     markup-content
            :reader   content)
   (range   :initarg  :range
            :reader   range
            :initform nil))
  (:default-initargs
   :content (error "missing required initarg")))

(defun make-hover-result (content &key range markup-kind)
  (let ((content (if markup-kind
                     (make-markup-content content markup-kind)
                     (make-markup-content content))))
    (make-instance 'hover-result :content content :range range)))

(defun unparse-hover-result (result)
  `((:contents  . ,(unparse-markup-content (content result)))
    ,@(when-let ((range (range result)))
        `((:range . ,(unparse-range range))))))

;;; Document Highlight

(define-enum highlight-kind
  (:text  1)
  (:read  2)
  (:write 3))

(define-message-class highlight (kind range)
  ((kind  :type highlight-kind)
   (range :type text.source-location:range)))

;;; Symbol Information

(define-enum symbol-information-kind
  (:file           1)
  (:module         2)
  (:namespace      3)
  (:package        4)
  (:class          5)
  (:method         6)
  (:property       7)
  (:field          8)
  (:constructor    9)
  (:enum          10)
  (:interface     11)
  (:function      12)
  (:variable      13)
  (:constant      14)
  (:string        15)
  (:number        16)
  (:boolean       17)
  (:array         18)
  (:object        19)
  (:key           20)
  (:null          21)
  (:enummember    22)
  (:struct        23)
  (:event         24)
  (:operator      25)
  (:typeparameter 26))

(defun unparse-symbol-information (uri version range name kind)
  `((:location . ((:uri     . ,uri)
                  (:version . ,version)
                  (:range   . ,(unparse-range range))))
    (:name     . ,name)
    (:kind     . ,(unparse-symbol-information-kind kind))))

;;; Diagnostic

(define-enum severity
  (:error   1)
  (:warning 2)
  (:note    3)
  (:info    4))

(defclass diagnostic ()
  ((annotation :initarg :annotation
               :reader  annotation)
   (message    :initarg :message
               :reader  message)))

(defun unparse-diagnostic (diagnostic)
  (etypecase diagnostic
    (diagnostic
     (let+ (((&accessors-r/o annotation message) diagnostic))
       `((:range    . ,(unparse-range (text.source-location:range annotation)))
         (:severity . ,(unparse-severity (text.source-location:kind annotation)))
         (:message  . ,(format nil "~A: ~A"
                               message (text.source-location:text annotation))))))
    (text.source-location::annotation
     `((:range    . ,(unparse-range (text.source-location:range diagnostic)))
       (:severity . ,(unparse-severity (text.source-location:kind diagnostic)))
       (:message  . ,(text.source-location:text diagnostic))))))

;;; Code Action

(define-message-class code-action (title command &rest arguments)
  ((title     :type     string)
   (command   :type     string)
   (arguments :type     (list-of string)
              :initform '())))
