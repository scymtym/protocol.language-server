(cl:in-package #:protocol.language-server.contributor)

;;; Diagnostics contributor protocol

(defgeneric diagnostics (workspace document)
  (:documentation
   "Return diagnostics for DOCUMENT in WORKSPACE."))

(defgeneric diagnostics-using-contributors (workspace document contributors)
  (:documentation
   "Return diagnostics for DOCUMENT in WORKSPACE using CONTRIBUTORS.

    The default method calls `diagnostics-contributions' for each
    element of CONTRIBUTORS and returns a (possibly empty) list of
    diagnostics."))

(defgeneric diagnostics-contributions (workspace document contributor)
  (:documentation
   "Return CONTRIBUTOR's diagnostics for DOCUMENT."))

;;; Default behavior

(defmethod diagnostics-using-contributors ((workspace    t)
                                           (document     t)
                                           (contributors list))
  (mappend (lambda (contributor)
             (with-simple-restart
                 (continue "~@<Skip diagnostics contributor ~A.~@:>"
                           contributor)
               (diagnostics-contributions workspace document contributor)))
           contributors))

;;; Context contributor protocol

(defgeneric contexts (workspace document position))

(defgeneric contexts-using-contributors (workspace document position contributors))

(defgeneric context-contributions (workspace document position contributor))

;;; Default behavior

(defmethod contexts-using-contributors ((workspace    t)
                                        (document     t)
                                        (position     t)
                                        (contributors list))
  (mappend (lambda (contributor)
             (with-simple-restart
                 (continue "~@<Skip context contributor ~A.~@:>" contributor)
               (context-contributions workspace document position contributor)))
           contributors))

;;; Hover contributor protocol

(defgeneric hover-using-contributors (workspace document contexts contributors))

(defgeneric hover-contribution (workspace document context contributor) ; TODO allow multiple per contributor
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod hover-using-contributors ((workspace    t)
                                     (document     t)
                                     (contexts     list)
                                     (contributors list))
  (let ((result-contents '())
        (result-range    nil))
    (map-product (lambda (contributor context)
                   (with-simple-restart
                       (continue "~@<Skip hover contributor ~A in context ~A.~@:>"
                                 contributor context)
                     (let+ (((&values contents range title) ; TODO multiple ranges? TODO should contributor return `proto:hover-result's and we merge them here?
                             (hover-contribution workspace document context contributor)))
                       (when contents
                         (when (not result-range) ; TODO if both, compare somehow
                           (setf result-range range))
                         (appendf result-contents (ensure-list (format nil "~@[## ~A~2%~]~A" title contents)))))))
                 contributors contexts)
    (when result-contents
      (proto:make-hover-result result-contents
                               :range       result-range
                               :markup-kind :markdown))))

;;; Signature contributor protocol

(defgeneric signatures-using-contributors (workspace document contexts contributors))

(defgeneric signature-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod signatures-using-contributors ((workspace    t)
                                          (document     t)
                                          (contexts     list)
                                          (contributors list))
  (let ((result-signatures       '())
        (result-active-signature nil)
        (result-active-parameter))
    (map-product (lambda (contributor context)
                   (with-simple-restart
                       (continue "~@<Skip signature help contributor ~A in context ~A.~@:>"
                                 contributor context)
                     (let+ (((&values signatures active-signature active-parameter)
                             (signature-contributions
                              workspace document context contributor)))
                       (appendf result-signatures signatures)
                       (unless result-active-signature
                         (setf result-active-signature active-signature
                               result-active-parameter active-parameter)))))
                 contributors contexts)
    (when result-signatures
      (proto::make-signature-help
       result-signatures result-active-signature result-active-parameter))))

;;; Document highlight contributor protocol

(defgeneric document-highlight-using-contributors (workspace document contexts contributors))

(defgeneric document-highlight-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod document-highlight-using-contributors ((workspace    t)
                                                  (document     t)
                                                  (contexts     list)
                                                  (contributors list))
  (flet ((one-contributor (contributor context)
           (with-simple-restart
               (continue "~@<Skip document highlight contributor ~A in context ~A.~@:>"
                         contributor context)
             (document-highlight-contributions
              workspace document context contributor))))
    (flatten (map-product #'one-contributor contributors contexts))))

;;; Completion contributor protocol

(defgeneric completions-using-contributors (workspace document contexts contributors))

(defgeneric completion-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod completions-using-contributors ((workspace    t)
                                           (document     t)
                                           (contexts     list)
                                           (contributors list))
  (flet ((one-contributor (contributor context)
           (with-simple-restart
               (continue "~@<Skip completion contributor ~A in context ~A.~@:>"
                         contributor context)
             (completion-contributions workspace document context contributor))))
    (flatten (map-product #'one-contributor contributors contexts))))

;;; Definition contributor protocol

(defgeneric definitions-using-contributors (workspace document context contributors))

(defgeneric definition-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod definitions-using-contributors ((workspace    t)
                                           (document     t)
                                           (contexts     list)
                                           (contributors list))
  (flet ((one-contributor (contributor context)
           (with-simple-restart
               (continue "~@<Skip definition contributor ~A in context ~A.~@:>"
                         contributor context)
             (definition-contributions workspace document context contributor))))
    (flatten (map-product #'one-contributor contributors contexts))))

;;; Reference contributor protocol

(defgeneric references-using-contributors (workspace document context contributors))

(defgeneric reference-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

;;; Default behavior

(defmethod references-using-contributors ((workspace    t)
                                          (document     t)
                                          (contexts     list)
                                          (contributors list))
  (flet ((one-contributor (contributor context)
           (with-simple-restart
               (continue "~@<Skip reference contributor ~A in context ~A.~@:>"
                         contributor context)
             (reference-contributions workspace document context contributor))))
    (flatten (map-product #'one-contributor contributors contexts))))

;;; Contributor creation protocol

(defgeneric make-contributors (document aspect)
  (:documentation
   "Return a list of contributors for ASPECT of document."))
