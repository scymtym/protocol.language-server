(cl:in-package #:protocol.language-server.visual-analyzer)

(clim:define-presentation-type document ())

(clim:define-presentation-method clim:present ((object t) (type document) stream view &key)
  (progn ; clim:surrounding-output-with-border (stream)
    (clim:with-drawing-options (stream :text-style (clim:make-text-style :fix nil :small))
      (let ((*print-level* 4)
            (*print-length* 20))
        (progn ; pprint-logical-block (stream (list object) :per-line-prefix "|")
          (princ object stream))))))

;;;

(clim:define-presentation-type message-kind ())

(clim:define-presentation-method clim:present ((object t) (type message-kind) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+green+ :text-style (clim:make-text-style :fix nil nil))
    (princ object stream)))

(clim:define-presentation-type request-id ())

(clim:define-presentation-method clim:present ((object t) (type request-id) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+blue+ :text-style (clim:make-text-style :fix :bold nil))
    (princ object stream)))

(clim:define-presentation-type interface ())

(clim:define-presentation-method clim:present ((object t) (type interface) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+red+ :text-style (clim:make-text-style :fix nil nil))
    (write-string object stream)))

(clim:define-presentation-type message ())

(clim:define-presentation-method clim:present ((object protocol.language-server.connection::error)
                                               (type   message)
                                               stream
                                               view
                                               &key)
  (clim:surrounding-output-with-border
      (stream :shape                :rounded
              :radius               16
              :radius-left          0
              :radius-bottom        0
              :background           clim:+lightpink+
              :highlight-background clim:+lightgray+
              :shadow               clim:+darkgray+)
    (clim:present "error" 'message-kind :stream stream :view view)
    (terpri)
    (write-string (protocol.language-server.connection::message object) stream)))

(clim:define-presentation-method clim:present ((object protocol.language-server.connection::notification)
                                               (type   message)
                                               stream
                                               view
                                               &key)
  (clim:surrounding-output-with-border
      (stream :shape                :rounded
              :radius               16
              :radius-left          0
              :radius-bottom        0
              :background           clim:+white+
              :highlight-background clim:+lightgray+
              :shadow               clim:+darkgray+)
    (clim:present "notification" 'message-kind :stream stream :view view)
    (write-string " " stream)
    (clim:present (protocol.language-server.connection::method object) 'interface
                  :stream stream :view view)
    (terpri stream)
    (loop :for (key value) :on (protocol.language-server.connection::arguments object) :by #'cddr
          :do (clim:present key 'message-kind :stream stream :view view)
              (format stream "~%  ")
              (clim:present value 'document :stream stream :view view)
              (terpri stream))))

(clim:define-presentation-method clim:present ((object protocol.language-server.connection::request)
                                               (type   message)
                                               stream
                                               view
                                               &key)
  (clim:surrounding-output-with-border
      (stream :shape                :rounded
              :radius               16
              :radius-left          0
              :background           clim:+white+
              :highlight-background clim:+lightgray+
              :shadow               clim:+darkgray+)
    (clim:present "request" 'message-kind :stream stream :view view)
    (write-string " " stream)
    (clim:present (protocol.language-server.connection::id object) 'request-id
                  :stream stream :view view)
    (write-string " " stream)
    (clim:present (protocol.language-server.connection::method object) 'interface
                  :stream stream :view view)
    (terpri stream)
    (loop :for (key value) :on (protocol.language-server.connection::arguments object) :by #'cddr
          :do (clim:present key 'message-kind :stream stream :view view)
              (format stream "~%  ")
              (clim:present value 'document :stream stream :view view)
              (terpri stream))))

(clim:define-presentation-method clim:present ((object protocol.language-server.connection::response)
                                               (type   message)
                                               stream
                                               view
                                               &key)
  (clim:surrounding-output-with-border
      (stream :shape                :rounded
              :radius               16
              :radius-right         0
              :background           clim:+white+
              :highlight-background clim:+lightgray+
              :shadow               clim:+darkgray+)
    (clim:present "response" 'message-kind :stream stream :view view)
    (write-string " " stream)
    (clim:present (protocol.language-server.connection::id object) 'request-id
                  :stream stream :view view)
    (terpri stream)
    (clim:present (protocol.language-server.connection::value object) 'document :stream stream :view view)))

(clim:define-presentation-method clim:present ((object t) (type message) stream view &key)
  (let ((message object))
    (clim:surrounding-output-with-border
        (stream :shape                :rounded
                :radius               16
                :radius-right         0
                :background           clim:+white+
                :highlight-background clim:+lightgray+
                :shadow               clim:+darkgray+)
      (clim:present (first message) 'message-kind :stream stream)
      (write-string " " stream)
      (clim:present (second message) 'request-id :stream stream)
      (write-string " " stream)
      (terpri stream)
      (case (first message)
        (:reply
         (clim:present (third message) 'request-id :stream stream)
         (terpri stream)
         (clim:present (fourth message) 'request-id :stream stream))))))

(clim:define-presentation-type event ())

(clim:define-presentation-method clim:present ((object t) (type event) stream view &key)
  (clim:with-drawing-options (stream :ink        clim:+dark-magenta+
                                     :text-style (clim:make-text-style nil nil :small))
    (format stream "@~A~2%" (timestamp object)))
  (clim:present (message object) 'message :stream stream :view view))

(defun display-event (event stream)
  (clim:present event 'event :stream stream))

#+not-needed? (defun display-trace (frame pane)
                #+no (map nil (lambda (event)
                                (display-event event pane)
                                (terpri pane))
                          (events (trace pane)))
                #+no  (let ((history (clim:stream-output-history pane)))
                        (map nil (lambda (event)
                                   (let ((record  (clim:with-output-to-output-record (pane)
                                                    (display-event event pane))))
                                     (setf (clim:output-record-position record)
                                           (values (if (typep (message event) '(or protocol.language-server.connection::request
                                                                                protocol.language-server.connection::notification))
                                                       16
                                                       (- (clim:bounding-rectangle-width pane)
                                                          (clim:bounding-rectangle-width record)
                                                          16))
                                                   (+ (clim:bounding-rectangle-max-y history) 16)))
                                     (clim:add-output-record record history)))
                             (events (trace pane)))
                        (clim:change-space-requirements pane :height (clim:bounding-rectangle-max-y history))
                        (let* ((viewport (climi::sheet-parent pane))
                               (amount   (- (clim:bounding-rectangle-height history)
                                            (clim:bounding-rectangle-height (or viewport pane)))))
                          (clim:scroll-extent pane 0 (max 0 amount)))))

;;; Thanks to jackdaniel in #clim
(defparameter *grid*
  (let ((array (make-array '(10 10) :initial-element 0)))
    (dotimes (v 10)
      (setf (aref array 0 v) 1
            (aref array v 0) 1))
    (let ((pattern (clim:make-pattern array (list clim:+white+ clim:+dark-grey+))))
      (clim:make-rectangular-tile pattern 10 10))))

(defclass trace-pane (clim:application-pane)
  ((%trace        :reader   trace
                  :accessor %trace)
   (%filter       :accessor filter
                  :initform nil)
   (%auto-scroll? :accessor auto-scroll?
                  :initform t))
  (:default-initargs
   ;; :display-function 'display-trace
   :display-time nil
   ;; :background       *grid*
   ))

(defmethod shared-initialize :after ((instance   trace-pane)
                                     (slot-names t)
                                     &key
                                     (trace nil trace-supplied?))
  (when trace-supplied?
    (setf (%trace instance) trace)))

;;;

(defmethod position-for-added-event ((container     trace-pane)
                                     (event         client->server-message)
                                     (output-record t))
  (let ((history (clim:stream-output-history container)))
    (values 16 (+ (clim:bounding-rectangle-max-y history) 16))))

(defmethod position-for-added-event ((container     trace-pane)
                                     (event         server->client-message)
                                     (output-record t))
  (let ((history (clim:stream-output-history container)))
    (values (- (clim:bounding-rectangle-width container)
               (clim:bounding-rectangle-width output-record)
               16)
            (+ (clim:bounding-rectangle-max-y history) 16))))

(defmethod scroll-to-bottom ((container trace-pane))
  (let* ((history  (clim:stream-output-history container))
         (viewport (climi::sheet-parent container))
         (amount   (- (clim:bounding-rectangle-height history)
                      (clim:bounding-rectangle-height (or viewport
                                                          container)))))
    (clim:scroll-extent container 0 (max 0 amount))))

(defmethod add-event! :around ((container trace-pane) (event t))
  (let ((filter (filter container)))
    (when (or (not filter) (funcall filter event))
      (call-next-method))))

(defmethod add-event! ((container trace-pane) (event t))
  (let ((history (clim:stream-output-history container))
        (record  (clim:with-output-to-output-record (container)
                   (display-event event container))))
    (setf (clim:output-record-position record)
          (position-for-added-event container event record))
    (clim:add-output-record record history)
    (clim:change-space-requirements container :height (clim:bounding-rectangle-max-y history))
    (when (auto-scroll? container)
      (scroll-to-bottom container))))

(defmethod clear! ((container trace-pane))
  (clear! (%trace container))
  (clim:clear-output-record (clim:stream-output-history container))
  (clim:scroll-extent container 0 0))

(defmethod redisplay ((container trace-pane))
  (clim:clear-output-record (clim:stream-output-history container))

  (let ((filter (filter container)))
    (map nil (lambda (event)
               (when (or (not filter) (funcall filter event))
                 (add-event! container event)))
         (events (%trace container)))))

;;; Filtering

(defmethod (setf filter) :after ((new-value t) (pane trace-pane))
  (redisplay pane))

;;;

(defmethod (setf %trace) :after (new-value (object trace-pane))
  (setf (change-hook new-value)
        (lambda (index)
          (clim:queue-event object (make-instance 'changed-event
                                                  :sheet object
                                                  :index index)))))

(defclass changed-event (climi::standard-event)
  ((index :initarg :index ; TODO carry the event instead of the index?
          :reader  index)))

(defmethod clim:handle-event ((client trace-pane)
                              (event  changed-event))
  (let* ((events  (events (%trace client)))
         (event   (aref events (index event))))
    (add-event! client event)))
