(cl:in-package #:protocol.language-server.visual-analyzer)

;;; `event'

(defclass event ()
  ((%timestamp :initarg :timestamp
               :reader  timestamp)
   (%message   :initarg :message
               :reader  message)))

(defclass client->server-message (event)
  ())

(defclass server->client-message (event)
  ())

(defclass debug-message (event)
  ())

;;; `trace'

(defclass trace ()
  ((%context     :initarg  :context
                 :accessor context)
   (%events      :reader   events
                 :initform (make-array 0 :adjustable t :fill-pointer 0))
   (%pairs       :initarg  pairs
                 :reader   pairs
                 :initform (make-hash-table))
   ;;
   (%change-hook :initarg  :change-hook
                 :type     list #| of function|#
                 :accessor change-hook
                 :initform '())))

;;;

(defmethod find-response ((request t) (container trace))
  nil)

(defmethod find-response ((request integer) (container trace))
  (when-let ((pair (gethash request (pairs container))))
    (cdr pair)))

(defmethod find-response ((request con:response) (container trace))
  (find-response (con:id request) container))

(defmethod find-response ((request event) (container trace))
  (find-response (message request) container))

(defmethod find-request ((response t) (container trace))
  nil)

(defmethod find-request ((response integer) (container trace))
  (when-let ((pair (gethash response (pairs container))))
    (car pair)))

(defmethod find-request ((response con:response) (container trace))
  (find-request (con:id response) container))

(defmethod find-request ((response event) (container trace))
  (find-request (message response) container))

;;;

(defmethod run-hook ((container trace) (event t) &rest args)
  (when-let ((change-hook (change-hook container)))
    (map nil (rcurry #'apply event args) change-hook)))

(defmethod (setf context) :after ((new-value t) (container trace))
  (run-hook container :context-changed new-value))

(defmethod add-event! ((container trace) (event t))
  (vector-push-extend event (events container)))

(defmethod add-event! :after ((container trace) (event t))
  ;; Link requests and responses
  (let ((message (message event)))
    (when (typep message 'con:id-message) ; TODO use dispatch to handle this
      (let* ((id   (con:id message))
             (cell (ensure-gethash id (pairs container) (cons nil nil))))
        (typecase message
          (con:request  (setf (car cell) event))
          (con:response (setf (cdr cell) event))))))

  ;; Run hook
  (run-hook container :event-added (1- (length (events container)))))

(defmethod clear! ((container trace))
  (setf (fill-pointer (events container)) 0)
  (clrhash (pairs container)))

(defmethod add-message! ((container trace)
                         (direction t)
                         (timestamp t)
                         (message   t))
  (let ((class (ecase direction
                 (:client->server 'client->server-message)
                 (:server->client 'server->client-message)
                 (:debug          'debug-message))))
    (add-event! container (make-instance class
                                         :timestamp timestamp
                                         :message   message))))

;; HACK
(defmethod add-message! ((container trace)
                         (direction t)
                         (timestamp t)
                         (message   con:call-message))
  (or (when-let* ((arguments (con:arguments message))
                  (uri       (when-let ((value (getf arguments :text-document))) ; TODO don't ignore version
                               (proto:parse-text-document value)))
                  (document  (lsp:find-document uri (lsp:workspace (context container)))))
        (let* ((position  (when-let ((value (getf arguments :position)))
                            (proto:parse value 'position)))
               (index     (when position
                            (sloc:index (sloc::attach-text position (lsp:text document)))))
               (range     (when-let ((value (getf arguments :range)))
                            (proto:parse value 'proto:range)))
               (range     (when range
                            (sloc::attach-text range (lsp:text document))))
               (arguments (append arguments
                                  (list :document document)
                                  (when range
                                    (list :range*  range
                                          :snippet (sloc:make-annotation
                                                    (make-instance
                                                     'sloc:location
                                                     :source (sloc:make-source (lsp:text document))
                                                     :range  range)
                                                    "here")))
                                  (when position
                                    (list :position* position
                                          :snippet   (sloc:make-annotation
                                                      (sloc:make-location
                                                       (lsp:text document)
                                                       index
                                                       (1+ index))
                                                      "here"))))))
          (reinitialize-instance message :arguments arguments))
        nil)
      (call-next-method)))
