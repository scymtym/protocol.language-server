;;;; connection.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.connection)

(defclass connection ()
  ((input  :initarg  :input
           :type     (satisfies input-stream-p)
           :reader   input)
   (output :initarg  :output
           :type     (satisfies output-stream-p)
           :reader   output))
  (:default-initargs
   :input  (error "missing initarg :input")
   :output (error "missing initarg :output")))

(defun make-connection (input output)
  (make-instance 'connection :input input :output output))

;; TODO this can be a request or a notification
(defmethod read-request ((connection connection))
  (let* ((raw       (transport:read-request (input connection)))
         (request   (json:decode-json-from-string raw))

         (id        (assoc-value request :id))
         (method    (assoc-value request :method))
         (arguments (alist-plist (assoc-value request :params))))
    (log:info "~@<=> ~:[     Notification~;~:*[~D] Request~] ~A~@:_~
               ~2@T~@<~/protocol.language-server.connection::print-maybe-alist/~:>~:>"
              id method arguments)
    (values id method arguments)))

(defmethod write-response ((connection connection) (id t) (payload t))
  (let* ((response (make-response id `(:result . ,payload)))
         (raw      (json:encode-json-to-string response)))
    (log:info "~@<<= [~D] Response~@:_~
               ~2@T~@<~/protocol.language-server.connection::print-maybe-alist/~:>~:>"
              id payload)
    (transport:write-response (output connection) raw)))

(defmethod write-response ((connection connection) (id t) (payload condition))
  (let* ((message  (princ-to-string payload))
         (payload  `(:error . ((:code    . 0) ; TODO code. search for "ErrorCodes" in spec
                               (:message . ,message))))
         (response (make-response id payload))
         (raw      (json:encode-json-to-string response)))
    (log:info "~@<<= [~D] Error Response~@:_~
               ~2@T~@<~/protocol.language-server.connection::print-maybe-alist/~:>~:>"
              id payload)
    (transport:write-response (output connection) raw)))

(defmethod write-notification ((connection connection) (method string) (payload t))
  (let* ((notification `((:jsonrpc . "2.0")
                         (:method  . ,method)
                         (:params  . ,payload)))
         (raw          (json:encode-json-to-string notification)))
    (log:info "~@<<=     Notification ~A~@:_~
               ~2@T~@<~/protocol.language-server.connection::print-maybe-alist/~:>~:>"
              method payload)
    (transport:write-response (output connection) raw)))

;;; Utilities

(defun make-response (id body)
  `((:jsonrpc . "2.0")
    (:id      . ,id)
    ,body))

