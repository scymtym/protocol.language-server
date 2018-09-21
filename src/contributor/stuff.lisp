(defclass variable-reference-completion-context (completion-context)
  ((%node  :initarg :node
           :reader  node)))

(defclass application-completion-context (completion-context)
  ((%node  :initarg :node
           :reader  node)))

(defclass application-operator-context (application-completion-context)
  ())

(defclass application-argument-context (application-completion-context)
  ())

(defclass block-reference-completion-context (completion-context)
  ((%node  :initarg :node
           :reader  node)))

(defclass type-reference-completion-context (completion-context)
  ((%node  :initarg :node
           :reader  node)))

(defclass literal-completion-context (completion-context)
  ((%node  :initarg :node
           :reader  node)
   (%value :initarg :value
           :reader  value)))

(defclass string-literal-completion-context (literal-completion-context)
  ())

(defclass pathname-literal-completion-context (literal-completion-context)
  ())

;; comment-context

(defmethod completion-context ((workspace t)
                               (document  toy-lisp-document)
                               (position  t))
  (reinitialize-instance position :column (1- (sloc:column position))) ; HACK
  (let+ (((&whole nodes first second &rest &ign)
          (nodes-at position 'language.toy.common-lisp.concrete-syntax::parse/common-lisp document)))
    (log:warn nodes)
    (cond (;; typep
           (and (typep first '(or abs:variable-reference abs:literal)) ; TODO make this extensible
                (typep second 'abs:application)
                (eq (second (abs:arguments second)) first)
                (eq (abs:name (abs:abstraction second)) 'cl:typep))
           (make-instance 'type-reference-completion-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ;; make-instance
          ((and (typep first '(or abs:variable-reference abs:literal)) ; TODO make this extensible
                (typep second 'abs:application)
                (eq (first (abs:arguments second)) first)
                (eq (abs:name (abs:abstraction second)) 'cl:make-instance))
           (make-instance 'type-reference-completion-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ;;
          ((and (typep first 'abs:variable-reference) ; TODO make this extensible
                (typep second 'abs:application)
                (eq (second (abs:arguments second)) first)
                (eq (abs:name (abs:abstraction second)) 'cl:return-from))
           (make-instance 'block-reference-completion-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ((and (typep first 'abs:variable-reference) ; TODO make this extensible
                (typep second 'abs:application)
                (eq (abs:abstraction second) first))
           (make-instance 'application-operator-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ((and (typep first 'abs:variable-reference) ; TODO make this extensible
                (typep second 'abs:application)
                (not (eq (abs:abstraction second) first)))
           (make-instance 'application-argument-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ((typep first 'abs:variable-reference)
           (make-instance 'variable-reference-completion-context
                          :position position
                          :nodes    (nodes-at position 'cps::convert document)
                          :node     first))

          ((and (typep first 'abs:literal)
                (typep (abs:value first) 'string))
           (make-instance 'string-literal-completion-context
                          :node  first
                          :value (abs:value first)))

          ((and (typep first 'abs:literal)
                (typep (abs:value first) 'pathname))
           (make-instance 'pathname-literal-completion-context
                          :node  first
                          :value (abs:value first))))))

(defvar *completion-contributors* '())

(defmacro define-completion-contributor
    (name ((context-var context-class)
           &optional (document-var 'document))
     &body body)
  `(progn
     (defclass ,name () ())

     (defmethod completions ((workspace     t)
                             (,document-var toy-lisp-document)
                             (,context-var  ,context-class)
                             (contributor   ,name))
       ,@body)

     (pushnew ',name *completion-contributors*)))

;;;

(define-completion-contributor lexical-variable-contributor
    ((context variable-reference-completion-context)
     document)
  (mappend (lambda (node)
             (log:warn node)
             (typecase node
               (cps:function-value
                (map 'list (lambda (variable)
                             (let ((name (cps:name variable)))
                               (proto:make-completion-item
                                (string-downcase name)
                                :kind          :variable
                                :detail        "parameter"
                                :documentation (format nil "documentation for ~A" name))))
                     (cps:parameters node)))
               (cps:bind-values
                (map 'list (lambda (variable)
                             (let ((name (cps:name variable)))
                               (proto:make-completion-item
                                (string-downcase name)
                                :range         (sloc:range (location-for-node (node context) document))
                                :kind          :variable
                                :detail        "lexical"
                                :documentation (format nil "documentation for ~A" name))))
                     (cps:variables node)))))
           (nodes context)))
