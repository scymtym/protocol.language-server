(cl:in-package #:protocol.language-server.contributor)

;;; Context contributor protocol

(defgeneric contexts (workspace document position))

(defgeneric contexts-using-contributors (workspace document position contributors))

(defgeneric context-contributions (workspace document position contributor))

;; If we had something like (adopt workspace document), we could call
;; this with a workspace argument
(defgeneric make-context-contributors (document))

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

(defgeneric make-hover-contributors (document))

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
      (proto:make-hover-result result-contents :range result-range))))

;;; Completion contributor protocol

(defgeneric completions-using-contributors (workspace document contexts contributors))

(defgeneric completion-contributions (workspace document context contributor)
  (:argument-precedence-order contributor context document workspace)
  (:method ((workspace t) (document t) (context t) (contributor t))
    '()))

(defgeneric make-completion-contributors (docuent))

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
