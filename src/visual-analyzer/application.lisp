(cl:in-package #:protocol.language-server.visual-analyzer)

(clim:define-command-table visual-analyzer
  :inherit-from (clouseau:inspector-command-table))

(clim:define-application-frame visualizer ()
  ((%trace :initarg  :trace
           :reader   trace))
  (:panes
   (context     (let ((pane  (clim:make-pane 'clouseau:inspector-pane))
                      (trace (trace clim:*application-frame*)))
                  (push (lambda (&rest event)
                          (destructuring-case event
                            ((:context-changed new-context)
                             (setf (clouseau:root-object
                                    pane :run-hook-p :if-changed)
                                   new-context))))
                        (change-hook trace))
                  pane))
   (context2    (let ((pane  (clim:make-pane 'clouseau::inspector-pane))
                      (trace (trace clim:*application-frame*)))
                  (push (lambda (&rest event)
                          (destructuring-case event
                            ((:context2-changed new-context)
                             (setf (clouseau:root-object
                                    pane :run-hook-p :if-changed)
                                   new-context))))
                        (change-hook trace))
                  pane))

   (auto-scroll clim:toggle-button
                :label "Scroll on event")
   (filter      clim:push-button
                :label "Filter")
   (trace       trace-pane
                :trace (trace clim:*application-frame*))

   (interactor  :interactor))
  (:layouts
   (all
    (clim:vertically (:width 900)
      (:fill (clim:horizontally ()
               (clim:vertically ()
                 (clim:scrolling () context)
                 (clim:make-pane 'clime:box-adjuster-gadget)
                 (clim:scrolling () context2))
               (clim:make-pane 'clime:box-adjuster-gadget)
               (clim:vertically ()
                 (clim:horizontally ()
                   auto-scroll
                   filter)
                 (:fill (clim:scrolling () trace)))))
      (clim:make-pane 'clime:box-adjuster-gadget)
                     (1/16  interactor)))
   (inspector
    (clim:vertically (:width 900)
      (:fill (clim:scrolling () context))
      (clim:make-pane 'clime:box-adjuster-gadget)
      (1/16  interactor)))
   (trace
    (clim:vertically (:width 900)
      (:fill (clim:vertically ()
               (clim:horizontally ()
                 auto-scroll
                 filter)
               (:fill (clim:scrolling () trace))))
      (clim:make-pane 'clime:box-adjuster-gadget)
      (1/16  interactor))))
  (:menu-bar nil)
  (:pointer-documentation t)

  (:command-table   (application :inherit-from (visual-analyzer)))
  (:command-definer nil)

  (:default-initargs
   :title  "Protocol Analyzer"
   :width  900
   :height 800))

(defmethod clim:frame-standard-output ((frame visualizer))
  (clim:find-pane-named frame 'interactor))

(defmethod clim:layout-frame :after ((frame visualizer) &optional width height)
  (declare (ignore width height))
  )

(clim:define-command (com-clear :name          "Clear"
                                :command-table visual-analyzer)
    ()
  (let ((trace-pane (clim:find-pane-named clim:*application-frame* 'trace)))
    (clear! trace-pane)))

(clim:define-command (com-set-filter :name          "Filter"
                                     :command-table visual-analyzer)
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

(clim:define-command (foo :command-table visual-analyzer)
    ((event event))
  )

(clim:define-presentation-to-command-translator
    event->foo (event foo visual-analyzer) (object)
  (list object))

(clim:define-command (inspect :name          "Inspect"
                              :command-table visual-analyzer)
    ((object trace-element :gesture :select))
  (clouseau:inspect object :new-process t))

(macrolet ((define (layout keystroke)
             (let ((name (symbolicate 'com-set-layout- layout)))
               `(clim:define-command (,name :command-table application
                                            :name          t
                                            :keystroke     ,keystroke)
                    ()
                  (setf (clim:frame-current-layout clim:*application-frame*)
                        ',layout)))))
  (define all       (#\1 :control))
  (define inspector (#\2 :control))
  (define trace     (#\3 :control)))

;;;

(defvar *trace*      nil)
(defvar *visualizer* nil)

(defun note-context (context)
  (when-let ((trace *trace*))
    (setf (context trace) context)))

(defun note-contrib-context (context)
  (when-let ((trace *trace*))
    (run-hook trace :context2-changed context)))

(defun add-message (direction message &key backtrace)
  (when-let ((trace *trace*))
    (add-message! trace direction (get-internal-real-time) message)))

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

(defun run-visualizer (&key (trace       (or *trace* (make-instance 'trace)))
                            (new-process t))
  (let ((frame (clim:make-application-frame 'visualizer :trace trace)))
    (setf *trace* trace *visualizer* frame)
    (if new-process
        (bt:make-thread (lambda ()
                          (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))
