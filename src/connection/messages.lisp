(cl:in-package #:protocol.language-server.connection)

;;;

(defclass message (print-items:print-items-mixin)
  ())

;;; `error'

(defclass error () ; TODO necessary?
  ((%code      :initarg :code
               :reader  code)
   (%message   :initarg :message
               :reader  message)
   (%backtrace :initarg :backtrace
               :reader  backtrace)))

(defun make-error (code message)
  (make-instance 'error :code code :message message))

(defmethod print-items:print-items append ((object error))
  `((:code    ,(code object)    "~D")
    (:message ,(message object) "/~A" ((:after :code)))))

;;; `call-message'

(defclass call-message (message)
  ((%method    :initarg :method
               :type    string
               :reader  method)
   (%arguments :initarg :arguments
               :reader  arguments)))

(defmethod print-items:print-items append ((object call-message))
  `((:method ,(method object) "~A")))

(defmethod to-alist ((message call-message))
  `((:jsonrpc . "2.0")
    (:method  . ,(method message))
    (:params  . ,(plist-alist (arguments message)))))

;;; `notification'

(defclass notification (call-message)
  ())

(defun make-notification (method &rest arguments &key &allow-other-keys)
  (make-instance 'notification
                 :method    method
                 :arguments arguments))

;;; `id-message'

(defclass id-message ()
  ((%id :initarg :id
        :reader  id)))

(defmethod print-items:print-items append ((object id-message))
  `((:id ,(id object) " [~D]" ((:after :method)))))

;;; `request'

(defclass request (id-message
                   call-message)
  ())

(defun make-request (id method &rest arguments &key &allow-other-keys)
  (make-instance 'request
                 :id        id
                 :method    method
                 :arguments arguments))

(defmethod to-alist ((message request))
  `((:jsonrpc . "2.0")
    (:id      . ,(id message))
    (:method  . ,(method message))
    (:params  . ,(plist-alist (arguments message)))))

;;; `response'

(defclass response (id-message)
  ((%result :initarg :result
            :reader  result)))

(defun make-response (id result)
  (make-instance 'response :id id :result result))

(defmethod to-alist ((message response))
  `((:jsonrpc . "2.0")
    (:id      . ,(id message))
    (:result  . ,(result message))))
