(cl:in-package #:protocol.language-server.contributor)

;;; `diagnostics-contributors-mixin'

(defclass diagnostics-contributors-mixin ()
  ((%diagnostics-contributors :reader diagnostics-contributors
                              :writer (setf %diagnostics-contributors))))

(defmethod initialize-instance :after
    ((instance diagnostics-contributors-mixin)
     &key
     (diagnostics-contributors (make-contributors instance 'diagnostics)))
  (setf (%diagnostics-contributors instance) diagnostics-contributors))

(defmethod diagnostics ((workspace t)
                        (document  diagnostics-contributors-mixin))
  (diagnostics-using-contributors
   workspace document (diagnostics-contributors document)))

;;; `context-contributors-mixin'

(defclass context-contributors-mixin ()
  ((%context-contributor :reader  context-contributors
                         :writer  (setf %context-contributors))))

(defmethod initialize-instance :after
    ((instance context-contributors-mixin)
     &key
     (context-contributors (make-contributors instance 'context)))
  (setf (%context-contributors instance) context-contributors))

(defmethod contexts ((workspace t)
                     (document  context-contributors-mixin)
                     (position  t))
  (contexts-using-contributors
   workspace document position (context-contributors document)))

;;; `hover-contributors-mixin'

(defclass hover-contributors-mixin ()
  ((%hover-contributors :reader  hover-contributors
                        :writer  (setf %hover-contributors))))

(defmethod initialize-instance :after
    ((instance hover-contributors-mixin)
     &key
     (hover-contributors (make-contributors instance 'hover)))
  (setf (%hover-contributors instance) hover-contributors))

(defmethod methods:hover ((workspace t)
                          (document  hover-contributors-mixin)
                          (position  t))
  (let ((contexts (contexts workspace document position)))
    (hover-using-contributors
     workspace document contexts (hover-contributors document))))

;;; `signature-contributors-mixin'

(defclass signature-contributors-mixin ()
  ((%signature-contributors :reader  signature-contributors
                            :writer  (setf %signature-contributors))))

(defmethod initialize-instance :after
    ((instance signature-contributors-mixin)
     &key
     (signature-contributors (make-contributors instance 'signature)))
  (setf (%signature-contributors instance) signature-contributors))

(defmethod methods:signature-help ((workspace t)
                                   (document  signature-contributors-mixin)
                                   (position  t))
  (let ((contexts (contexts workspace document position)))
    (signatures-using-contributors
     workspace document contexts (signature-contributors document))))

;;; `completion-contributors-mixin'

(defclass completion-contributors-mixin ()
  ((%completion-contributors :reader  completion-contributors
                             :writer  (setf %completion-contributors))))

(defmethod initialize-instance :after
    ((instance completion-contributors-mixin)
     &key
     (completion-contributors (make-contributors instance 'completion)))
  (setf (%completion-contributors instance) completion-contributors))

(defmethod methods:completion ((workspace t)
                               (document  completion-contributors-mixin)
                               (position  t))
  (let ((contexts (contexts workspace document position)))
    (completions-using-contributors
     workspace document contexts (completion-contributors document))))

;;; `definition-contributors-mixin'

(defclass definition-contributors-mixin ()
  ((%definition-contributors :reader  definition-contributors
                             :writer  (setf %definition-contributors))))

(defmethod initialize-instance :after
    ((instance definition-contributors-mixin)
     &key
     (definition-contributors (make-contributors instance 'definition)))
  (setf (%definition-contributors instance) definition-contributors))

(defmethod methods:definition ((workspace t)
                               (document  definition-contributors-mixin)
                               (position  t))
  (let ((contexts (contexts workspace document position)))
    ;; TODO maybe try to take the most specific location
    (first (definitions-using-contributors
            workspace document contexts (definition-contributors document)))))

;;; `reference-contributors-mixin'

(defclass reference-contributors-mixin ()
  ((%reference-contributors :reader  reference-contributors
                            :writer  (setf %reference-contributors))))

(defmethod initialize-instance :after
    ((instance reference-contributors-mixin)
     &key
     (reference-contributors (make-contributors instance 'reference)))
  (setf (%reference-contributors instance) reference-contributors))

(defmethod methods:references ((workspace            t)
                               (document             reference-contributors-mixin)
                               (position             t)
                               (include-declaration? t))
  ;; TODO respect include-declaration?
  (let ((contexts (contexts workspace document position)))
    (references-using-contributors
     workspace document contexts (reference-contributors document))))
