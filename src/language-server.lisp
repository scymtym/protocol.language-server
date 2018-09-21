;;;; language-server.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defvar *debug* nil)

(defun language-server (input output)
  (let* ((connection (protocol.language-server.connection:make-connection
                      input output))
         (context    (make-instance 'context :connection connection)))
    (process-requests connection context)))

(defun process-requests (connection context)
  (log4cl:remove-all-appenders log4cl:*root-logger*)
  (log:config :daily "/tmp/language-server.log")
  (log:config :info)
  (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
    (catch 'exit
      (loop (process-request connection context)))))

(define-condition diagnostic (condition)
  ((uri         :initarg :uri
                :reader  uri
                :writer  (setf %uri))
   (diagnostics :initarg :diagnostics
                :reader  diagnostics)))

(define-condition message (condition)
  ((message :initarg :message
            :reader  message)))

(defun process-request (connection context)
  (let+ (((&values id method arguments message)
          (protocol.language-server.connection:read-request connection))
         (diagnostics (make-hash-table :test #'eq))
         (messages    '())
         ((&values result condition)
          (restart-case
              (block nil
                (handler-bind
                    ((error      (lambda (condition)
                                   (if *debug*
                                       (invoke-debugger condition)
                                       (return (values nil condition)))))
                     (diagnostic (lambda (condition) ; TODO generalize to all notifications
                                   (appendf (gethash (uri condition) diagnostics '())
                                            (diagnostics condition))))
                     (message    (lambda (condition)
                                   (appendf messages (list (message condition))))))
                  (apply #'process-method context method arguments)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Abort processing the current request.~@:>"))
              (values nil (or condition
                              (make-condition 'simple-error
                                              :format-control   "~@<Request aborted.~@:>"
                                              :format-arguments '())))))))


    (with-simple-restart (continue "~@<Skip this shit.~@:>")
      (when (find-package '#:protocol.language-server.visual-analyzer)
        (handler-bind
            ((error (lambda (condition) (log:error condition) )))
          (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:note-context context)
          (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:add-message :client->server message)
          (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:add-message :server->client
                            (cond (condition
                                   (make-instance 'protocol.language-server.connection::error
                                                  :code    0
                                                  :message (princ-to-string condition)))
                                  (id
                                   (make-instance 'protocol.language-server.connection::response
                                                  :id    id
                                                  :value result))))
          #+TODO (loop :for diagnostic
                          (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:add-event
                                            (make-instance 'protocol.language-server.connection::notification
                                                           :method )))
          #+LATER (map nil (lambda (message)
                     (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:add-message
                                       :server->client message))
               messages))))

    (format *trace-output* "Method: ~S/~S~%Result: ~S Condition: ~S~%"
            method id result condition)

    (when condition          ; TODO (handle-error context condition) ?
      (log:error "~A: ~A" (type-of condition) condition)
      (format *trace-output* "Error: ~A~%" condition)
      (force-output *trace-output*))

    (maphash
     (lambda (uri diagnostics)
       (protocol.language-server.connection:write-notification
        connection "textDocument/publishDiagnostics"
        `((:uri         . ,uri)
          (:diagnostics . ,(map 'vector #'proto:unparse-diagnostic
                                diagnostics)))))
     diagnostics)

    (map nil (lambda (message)
               (protocol.language-server.connection:write-notification
                connection "window/logMessage"
                (proto:unparse-message message)))
         messages)

    (cond
      ((eq result :exit)
       (throw 'exit nil))
      ((not id))
      (t
       (protocol.language-server.connection:write-response
        connection id result)))))
