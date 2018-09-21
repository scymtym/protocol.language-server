(cl:in-package #:protocol.language-server.connection)

;;;

(defclass message (print-items:print-items-mixin)
  ())

;;; `error'

(defclass error (message)
  ((code    :initarg :code
            :reader  code)
   (message :initarg :message
            :reader  message)))

(defmethod print-items:print-items append ((object error))
  `((:code    ,(code object)    "~D")
    (:message ,(message object) "/~A" ((:after :code)))))

;;; `call-message'

(defclass call-message (message)
  ((method    :initarg :method
              :type    string
              :reader  method)
   (arguments :initarg :arguments
              :reader  arguments)))

(defmethod print-items:print-items append ((object call-message))
  `((:method ,(method object) "~A")))

(defmethod to-alist ((message call-message))
  `((:jsonrpc . "2.0")
    (:method  . ,(method message))
    (:params  . ,(arguments message))))

;;; `notification'

(defclass notification (call-message)
  ())

;;; `id-message'

(defclass id-message ()
  ((id :initarg :id
       :reader  id)))

(defmethod print-items:print-items append ((object id-message))
  `((:id ,(id object) " [~D]" ((:after :method)))))

;;; `request'

(defclass request (id-message
                   call-message)
  ())

;;; `response'

(defclass response (id-message)
  ((value :initarg :value
          :reader  value)))

(defmethod to-alist ((message response))
  `((:jsonrpc . "2.0")
    (:id      . ,(id message))
    ,(value message)))

(make-instance 'error :code 2 :message "bla bla")
(make-instance 'request :method "textDocument/hover" :id 6)
(to-alist (make-instance 'notification :method "textDocument/hover" :arguments '()))
