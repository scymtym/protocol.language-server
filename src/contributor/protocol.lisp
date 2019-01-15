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
                     (let+ (((&values contents range) ; TODO multiple ranges? TODO should contributor return `proto:hover-result's and we merge them here?
                             (hover-contribution workspace document context contributor)))
                       (log:warn contributor contents range)
                       (when contents
                         (when (not result-range) ; TODO if both, compare omehow
                           (setf result-range range))
                         (appendf result-contents contents)))))
                 contributors contexts)
    (when result-contents
      (proto:make-hover-result result-contents
                               :range       result-range
                               :markup-kind :markdown))))

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
  (flatten (map-product (lambda (contributor context)
                          (with-simple-restart
                              (continue "~@<Skip completion contributor ~A in context ~A.~@:>"
                                        contributor context)
                            (completion-contributions workspace document context contributor)))
                        contributors contexts)))

;;; Contributor creation protocol

;; TODO If we had something like (adopt workspace document), we could call
;; this with a workspace argument
(defgeneric make-contributors (document aspect)
  (:documentation
   "Return a list of contributors for ASPECT of document."))
