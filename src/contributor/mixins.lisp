(cl:in-package #:protocol.language-server.contributor)

;;; `diagnostics-contributors-mixin'

(defclass diagnostics-contributors-mixin ()
  ((%diagnostics-contributors :reader diagnostics-contributors
                              :writer (setf %diagnostics-contributors))))

(defmethod initialize-instance :after
    ((instance diagnostics-contributors-mixin)
     &key
     (diagnostics-contributors (make-diagnostics-contributors instance)))
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
     (context-contributors (make-context-contributors instance)))
  (setf (%context-contributors instance) context-contributors))

(defmethod contexts ((workspace t)
                     (document  context-contributors-mixin)
                     (position  t))
  (protocol.language-server::debug1
   (contexts-using-contributors
    workspace document position (context-contributors document))))

;;; `hover-contributors-mixin'

(defclass hover-contributors-mixin ()
  ((%hover-contributors :reader  hover-contributors
                        :writer  (setf %hover-contributors))))

(defmethod initialize-instance :after
    ((instance hover-contributors-mixin)
     &key
     (hover-contributors (make-hover-contributors instance)))
  (setf (%hover-contributors instance) hover-contributors))

(defmethod methods:hover ((workspace t)
                          (document  hover-contributors-mixin)
                          (position  t))
  (let ((contexts (contexts workspace document position)))
    (hover-using-contributors
     workspace document contexts (hover-contributors document))))

;;; `completion-contributors-mixin'

(defclass completion-contributors-mixin ()
  ((%completion-contributors :reader  completion-contributors
                             :writer  (setf %completion-contributors))))

(defmethod initialize-instance :after
    ((instance completion-contributors-mixin)
     &key
     (completion-contributors (make-completion-contributors instance)))
  (setf (%completion-contributors instance) completion-contributors))

(defmethod methods:completion ((workspace t)
                               (document  completion-contributors-mixin)
                               (position  t))
  (let ((contexts (contexts workspace document position)))
    (completions-using-contributors
     workspace document contexts (completion-contributors document))))
