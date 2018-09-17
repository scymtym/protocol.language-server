;;;; messages.lisp --- Messages specified by the language server protocol.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

;; TODO make parse and unparse generic functions

;;; Capabilities

(define-enum text-document-sync-kind
  (:none        0)
  (:full        1)
  (:incremental 2))

(define-message-class completion-options
    (&optional (resolve-provider t) trigger-characters)
  ((resolve-provider   :type boolean)
   (trigger-characters :type (list-of string))))

(define-message-class signature-help-options ()
  ())

(define-message-class execute-command-options (commands)
  ((commands :type (list-of string))))

(define-message-class server-capabilities
    (&key
     (text-document-sync                   :incremental)
     (hover-provider                       t)
     (completion-provider                  (make-completion-options))
     (signature-help-provider              (make-signature-help-options))
     (definition-provider                  t)
     (references-provider                  t)
     (document-highlight-provider          t)
     (document-symbol-provider             t)
     (workspace-symbol-provider            nil)
     (code-action-provider                 nil)
     (document-formatting-provider         nil)
     (document-range-formatting-provider   nil)
     (document-on-type-formatting-provider nil)
     (rename-provider                      t)
     (execute-command-provider             nil))
    ((text-document-sync                   :type text-document-sync-kind)
     (hover-provider                       :type boolean)
     (completion-provider                  :type (or null completion-options))

     (signature-help-provider              :type (or null signature-help-options))

     (definition-provider                  :type boolean)
     ; typeDefinitionProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
     ; implementationProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);

     (references-provider                  :type boolean)
     (document-highlight-provider          :type boolean)
     (document-symbol-provider             :type boolean)
     (workspace-symbol-provider            :type boolean)
     (code-action-provider                 :type boolean)
     ; (code-lens-provider                   :type (or null code-lens-options))
     (document-formatting-provider         :type boolean)
     (document-range-formatting-provider   :type boolean)
     (document-on-type-formatting-provider :type (or null document-on-type-formatting-options))
     (rename-provider                      :type boolean)
     ; (document-link-provider               :type (or null document-link-options))
     ; (color-provider                      :type boolean | Color-Provider-Options | (Color-Provider-Options & Text-Document-Registration-Options & Static-Registration-Options))
     ; (folding-range-provider              :type boolean | Folding-Range-Provider-Options | (Folding-Range-Provider-Options & Text-Document-Registration-Options & Static-Registration-Options))
     (execute-command-provider             :type (or null execute-command-options))

     ;; workspace : {
     ;; /**
     ;; * The server supports workspace folder.
     ;; *
     ;; * Since 3.6.0
     ;; */
     ;; workspace-Folders : {
     ;; /**
     ;; * The server has support for workspace folders
     ;; */
     ;; supported : boolean;
     ;; /**
     ;; * Whether the server wants to receive workspace folder
     ;; * change notifications.
     ;; *
     ;; * If a strings is provided the string is treated as a ID
     ;; * under which the notification is registered on the client
     ;; * side. The ID can be used to unregister for these events
     ;; * using the `client/unregister-Capability` request.
     ;; */
     ;; change-Notifications : string | boolean;
     ;; }
     ;; }

     ))

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

;;;

(define-message-class versioned-text-document-identifier (uri version)
  ((uri     :type string)
   (version :type non-negative-integer)))

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

;;; {Show,Log}Message

(define-enum message-kind
  (:error   1)
  (:warning 2)
  (:info    3)
  (:log     4))

(define-message-class message-action-item (title)
  ((title :type string)))

(define-message-class message (kind message &rest action-items)
  ((kind         :type message-kind)
   (message      :type string)
   (action-items :type (list-of message-action-item))))

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

#+TODO-later (define-message-class completion-item (label &rest args
                                       &key kind filter-text detail documentation
                                       range (new-text label) text-format)
  (;; Presentation
   (label         :type     string)
   (kind          :type     (or null completion-item-kind))
   (filter-text   :type     (or null string))
   (detail        :type     (or null string))
   (documentation :type     (or null string)
                  :reader   documentation*)
   ;; Replacement text
   (edit          :type     edit
                  :accessor %edit)
   (text-format   :type     (or null insert-text-format)
                  :initform :plain-text)))

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

(defclass hover-result () ; TODO rename to just hover?
  ((content :initarg  :content
            :type     (cons markup-content)
            :reader   content)
   (range   :initarg  :range
            :reader   range
            :initform nil))
  (:default-initargs
   :content (error "missing required initarg")))

(defun make-hover-result (content &key range markup-kind)
  (let+ (((&flet make-content (content)
            (if markup-kind
                (make-markup-content content markup-kind)
                (make-markup-content content))))
         (content (if (listp content)
                      (map 'list #'make-content content)
                      (list (make-content content)))))
    (make-instance 'hover-result :content content :range range)))

(defun unparse-hover-result (result)
  `((:contents  . ,(map 'vector #'unparse-markup-content (content result)))
    ,@(when-let ((range (range result)))
        `((:range . ,(unparse-range range))))))

;;; Document Highlight

(define-enum highlight-kind
  (:text  1)
  (:read  2)
  (:write 3))

(define-message-class highlight (kind range)
  ((kind  :type highlight-kind)
   (range :type text.source-location:range))) ; TODO sloc:location should also work

(defmethod print-object ((object highlight) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A @ ~/print-items:format-print-items/"
            (kind object)
            (print-items:print-items (range object)))))

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

#+TODO-later (define-message-class symbol-information (location name kind)
  ((location :type text.source-location:location)
   (name     :type string)
   (kind     :type symbol-information-kind)))

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
  ((annotation  :initarg :annotation
                :reader  annotation)
   (annotations :initarg :annotations
                :type    (list-of text.source-location::annotation))
   (message     :initarg :message
                :reader  message)))

; (defun make-diagnostic )

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

;;; Code Lens

(define-message-class code-lens (range &optional command data)
  ((range   :type     text.source-location:range)
   (command :type     string ; (or null string)
            :initform nil)
   (data    :type     t
            :initform nil)))

;;; Document Link

(define-message-class document-link (range &optional target)
    ((range  :type     text.source-location:range)
     (target :type     string ; (or null string)
             :initform nil)))
