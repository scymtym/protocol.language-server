;;;; messages.lisp --- Messages specified by the language server protocol.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

;; TODO make parse and unparse generic functions

(defconstant +false+ '+false+)

(defmethod cl-json:encode-json ((object (eql +false+)) &optional stream)
  (write-string "false" stream))

;;; Capabilities

(define-enum text-document-sync-kind
  (:none        0)
  (:full        1)
  (:incremental 2))

(define-message-class completion-options
    (&optional (resolve-provider t) trigger-characters)
  ((resolve-provider   :type boolean)
   (trigger-characters :type (list-of string))))

(define-message-class signature-help-options (&key (trigger-characters '()))
  ((trigger-characters :type (list-of string))))

(define-message-class document-on-type-formatting-options ()
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

(define-message-class workspace-client-capabilities ()
  ())

(define-message-class text-document-client-capabilities ()
  ())

(define-message-class client-capabilities (&key workspace text-document experimental)
  ((workspace     :type (or null workspace-client-capabilities))
   (text-document :type (or null text-document-client-capabilities))
   (experimental  :type t)))

;;; Position type

(defmethod parse ((data t) (message-class (eql 'position)))
  (make-instance 'text.source-location::line+column-position
                 :line   (expect-property data :line      'non-negative-integer)
                 :column (expect-property data :character 'non-negative-integer)))

(defmethod unparse ((message text.source-location::line+column-position))
  `((:line      . ,(text.source-location:line   message))
    (:character . ,(text.source-location:column message))))

#+sbcl
(declaim (sb-ext:deprecated :early ("protocol.language-server" "0.1")
                            (function parse-position :replacement parse)
                            (function unparse-position :replacement unparse)))

(defun parse-position (object)
  (parse object 'position))

(defun unparse-position (position)
  (unparse position))

;;; Range type

(defmethod parse ((data t) (message-class (eql 'text.source-location:range)))
  (text.source-location:make-range
   (parse (expect-property data :start 'cons) 'position)
   (parse (expect-property data :end   'cons) 'position)))

(defmethod unparse ((message text.source-location:range))
  `((:start . ,(unparse (text.source-location:start message)))
    (:end   . ,(unparse (text.source-location:end message)))))

#+sbcl
(declaim (sb-ext:deprecated :early ("protocol.language-server" "0.1")
                            (function parse-range :replacement parse)
                            (function unparse-range :replacement unparse)))

(defun parse-range (object)
  (parse object 'text.source-location:range))

(defun unparse-range (range)
  (unparse range))

;;; Location type

(defmethod parse ((data t) (message-class (eql 'text.source-location:location)))
  (let ((uri   (expect-property data :uri   'string))
        (range (expect-property data :range 'cons)))
    (make-instance 'text.source-location:location
                   :source (text.source-location:make-source uri)
                   :range  (parse range 'text.source-location:range))))

(defmethod unparse ((message text.source-location:location))
  (let ((name (text.source-location:name
               (text.source-location:source message))))
    `((:uri   . ,(typecase name
                   (pathname (format nil "file://~A" name))
                   (t        name)))
      (:range . ,(unparse (text.source-location:range message))))))

#+sbcl
(declaim (sb-ext:deprecated :early ("protocol.language-server" "0.1")
                            (function parse-location :replacement parse)
                            (function unparse-location :replacement unparse)))

(defun parse-location (object)
  (parse object 'text.source-location:location))

(defun unparse-location (location)
  (unparse location))

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
          (parse range 'text.source-location:range))
        (when-let ((range-length (maybe-property thing :range--length 'string))) ; TODO why string?
          (parse-integer range-length))))

;;; Edit

(define-message-class edit (range new-text)
  ((range    :type text.source-location:range)
   (new-text :type string)))

;;; Text document edit

(define-message-class text-document-edit (document &rest edits)
  ((document :type versioned-text-document-identifier
             :property :text-document)
   (edits    :type (list-of edit))))

;;; Workspace edit

(define-message-class workspace-edit (document-changes ; &optional changes
                                      )
  (; (changes          :type (list-of text-document-edit))
   (document-changes :type (list-of text-document-edit))))

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

;; TODO in case of markup-content, can only have a single content
;; TODO rename content -> contents
(defclass hover-result ()               ; TODO rename to just hover?
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
  (let ((kind  (kind (first (content result))))
        (value (format nil "~{~A~^~2%~}"
                       (map 'list #'value (content result)))))
   `((:contents  . ,(unparse-markup-content (make-markup-content value kind)))
     ,@(when-let ((range (range result)))
         `((:range . ,(unparse range)))))))

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

(define-enum symbol-kind
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
  ;; Later additions
  (:object        19)
  (:key           20)
  (:null          21)
  (:enummember    22)
  (:struct        23)
  (:event         24)
  (:operator      25)
  (:typeparameter 26))

(define-message-class document-symbol (name kind range
                                       &key
                                       detail
                                       deprecated?
                                       (selection-range range)
                                       children)
    ((name            :type string)
     (detail          :type (or null string))
     (kind            :type symbol-kind)
     (deprecated?     :type boolean) ; TODO is mapped to JSON correctly w.r.t. the spec?
     (range           :type text.source-location:range)
     (selection-range :type text.source-location:range)
     (children        :type (list-of document-symbol))))

(defmethod print-items:print-items append ((object document-symbol))
  (let ((kind  (kind object))
        (name  (name object))
        (range (print-items:print-items (range object))))
    `((:kind  ,kind  "~A")
      (:name  ,name  " ~A" ((:after :kind)))
      (:range ,range " @~/print-items:format-print-items/" ((:after :name))))))

(define-message-class symbol-information (name kind location &key deprecated? container-name)
  ((name           :type string)
   (kind           :type symbol-kind)
   (deprecated?    :type boolean)
   (location       :type text.source-location:location)
   (container-name :type (or null string))))

;;; Diagnostic

(define-enum severity
  (:error   1)
  (:warning 2)
  (:note    3)
  (:info    4))

#+later (define-message-class diagnostic (message &rest annotations)
  ((annotations :initarg :annotations
                :type    (list-of text.source-location:annotation))
   (message     :initarg :message
                :reader  message)))

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
       `((:range    . ,(unparse (text.source-location:range annotation)))
         (:severity . ,(unparse-severity (text.source-location:kind annotation)))
         (:message  . ,(format nil "~A: ~A"
                               message (text.source-location:text annotation))))))
    (text.source-location:annotation
     `((:range    . ,(unparse (text.source-location:range diagnostic)))
       (:severity . ,(unparse-severity (text.source-location:kind diagnostic)))
       (:message  . ,(text.source-location:text diagnostic))))))

;;; Command

(define-message-class command (title command &rest arguments)
  ((title     :type     string)
   (command   :type     string)
   (arguments :type     (list-of string)
              :initform '())))

;;; Code Action

(define-enum code-action-kind
  (:quickfix                "quickfix")

  (:refactor                "refactor")
  (:refactor.extract        "refactor.extract")
  (:refactor.inline         "refactor.inline")
  (:refactor.rewrite        "refactor.rewrite")

  (:source                  "source")
  (:source.organize-imports "source.organizeImports"))

(define-message-class code-action (title kind command &rest arguments)
  ((title     :type     string)
   (kind      :type     code-action-kind)
   ; TODO edit
   (command   :type     command)
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
