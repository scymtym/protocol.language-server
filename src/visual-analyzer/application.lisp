(cl:in-package #:protocol.language-server.visual-analyzer)

(clim:define-application-frame visualizer ()
  ((%trace :initarg  :trace
           :reader   trace))
  (:panes
   ; (auto-scroll clim:toggle-button)
   (trace                 trace-pane)
   (interactor            :interactor)
   (pointer-documentation :pointer-documentation))
  (:layouts
   (default
    (clim:vertically ()
      (:fill (clim:vertically ()
                              ;; auto-scroll
               (clim:scrolling () trace)))
      (1/16  interactor)
      (1/32  pointer-documentation))))
  (:default-initargs
   :title  "Protocol Analyzer"
   :width  900
   :height 800))

(defmethod clim:layout-frame :after ((frame visualizer) &optional width height)
  (let ((auto-scroll (clim:find-pane-named frame 'auto-scroll))
        (trace       (clim:find-pane-named frame 'trace)))
    (setf (%trace trace) (trace frame))
    #+no (reinitialize-instance
     auto-scroll
     :value-changed-callback (lambda (value)
                               (setf (auto-scroll? trace) value)))))

(define-visualizer-command (clear :name "Clear") ()
  (let ((trace-pane (clim:find-pane-named clim:*application-frame* 'trace)))
    (clear! trace-pane)))

(define-visualizer-command (com-set-filter :name "Filter")
    ((expression '(or (member :client->server :server->client)
                      string
                      clim:form)))
  (let ((trace-pane (clim:find-pane-named clim:*application-frame* 'trace)))
    (setf (filter trace-pane)
          (typecase expression
            ((eql :client->server) (rcurry #'typep 'client->server-message))
            ((eql :server->client) (rcurry #'typep 'server->client-message))
            (string                (lambda (event)
                                     (flet ((matching-request? (message)
                                              (and (typep message 'con:call-message)
                                                   (search expression (con:method message)))))
                                       (let ((message (message event)))
                                         (or (matching-request? message)
                                             (and (typep message 'con:response)
                                                  (when-let ((request (find-request event (trace trace-pane))))
                                                    (matching-request? (message request)))))))))
            (t                     (compile nil `(lambda (cl-user::event) ,expression)))))))

(define-visualizer-command foo ((event event))
  )

(clim:define-presentation-to-command-translator
    event->foo (event foo visualizer) (object)
  (list object))

(define-visualizer-command inspect ((object t))
  (uiop:symbol-call '#:clouseau '#:inspector object :new-process t))

(clim:define-presentation-to-command-translator
    document->inspect (document inspect visualizer) (object)
  (list object))

;;;

(defvar *trace*      nil)
(defvar *visualizer* nil)

(defun note-context (context)
  (when *trace*
    (setf (context *trace*) context)))

(defun add-message (direction message)
  (when *trace*
    (add-message! *trace* direction (get-internal-real-time) message)))

;; (defun %add-event (class message)
;;   (when *trace*
;;     (add-message!
;;      (make-instance
;;       (ecase direction
;;         (:client->server 'client->server-message)
;;         (:server->client 'server->client-message))
;;       :timestamp (get-internal-real-time)
;;       :message   message))
;;     (add-event! *trace* )))
;;
;;
;; (defun add-event (message)
;;   (%add-event 'event message))
;;
;; (defun add-client->server-message (message)
;;   (%add-event 'client->server-message message))
;;
;; (defun add-server->client-message (message)
;;   (%add-event 'server->client-message message))

(defun start ()
  (let* ((trace (or *trace* (make-instance 'trace)))
         (frame (clim:make-application-frame 'visualizer :trace trace)))
    (setf *trace* trace *visualizer* frame)
    (bt:make-thread (lambda ()
                      (clim:run-frame-top-level frame)))))

(uiop:register-image-restore-hook 'start nil)
