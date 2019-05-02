;;;; language-server.lisp --- Entry point and message processing loop.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defvar *debug* nil)

(defun language-server (input output
                        &key
                        (connection-class nil      connection-class-supplied?)
                        (context-class    'context context-class-supplied?)
                        (capabilities     '()))
  (let* ((connection (apply #'conn:make-connection input output
                            (when connection-class-supplied?
                              (list :class connection-class))))
         (context    (apply #'make-context connection
                            :capabilities capabilities
                            (when context-class-supplied?
                              (list :class context-class)))))
    (process-requests connection context)))

(defun process-requests (connection context)
  (log4cl:remove-all-appenders log4cl:*root-logger*)
  (log:config :daily "/tmp/language-server.log")
  (log:config :info)
  (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
    (catch 'exit
      (loop (with-simple-restart (continue "~@<Skip this message.~@:>")
              (process-request connection context))))))

(define-condition diagnostic (condition)
  ((uri         :initarg :uri
                :reader  uri
                :writer  (setf %uri))
   (diagnostics :initarg :diagnostics
                :reader  diagnostics)))

(define-condition message (condition)
  ((message :initarg :message
            :reader  message)))

(define-condition debug1 (condition)
  ((message :initarg :message
            :reader  message)))

(defun debug1 (message)
  (signal 'debug1 :message message)
  message)

(defun process-request (connection context)
  (with-simple-restart (continue "~@<Skip this shit.~@:>")
    (when (find-package '#:protocol.language-server.visual-analyzer)
      (handler-bind
          ((error (lambda (condition)
                    (log:error "Error submitting message to visual analyzer: ~A" condition)
                    (log:error (with-output-to-string (stream)
                                 (sb-debug:print-backtrace :stream stream))))))
        (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:note-context context))))

  (let+ (((&values message id method arguments) (conn:read-message connection))
         (diagnostics (make-hash-table :test #'eq))
         ((&values result condition backtrace)
          (restart-case
              (block nil
                (handler-bind
                    ((debug1     (lambda (condition)
                                   (conn::submit-to-visual-analyzer :debug (message condition)))))
                  (handler-bind
                      ((error      (lambda (condition)
                                     (if *debug*
                                         (invoke-debugger condition)
                                         (return (values nil condition #+no (sb-debug:list-backtrace) (with-output-to-string (stream)
                                                                                                        (sb-debug:print-backtrace :stream stream)))))))
                       (diagnostic (lambda (condition) ; TODO generalize to all notifications
                                     (appendf (gethash (uri condition) diagnostics '())
                                              (diagnostics condition))))
                       (message    (lambda (condition)
                                     ;; TODO does not go into visual analyzer this way
                                     (conn:write-message
                                      connection (apply #'conn:make-notification
                                                        "window/logMessage" (alist-plist (proto:unparse (message condition))))))))
                    (when method
                      (apply #'process-method context method arguments)))))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Abort processing the current request.~@:>"))
              (values nil (or condition
                              (make-condition 'simple-error
                                              :format-control   "~@<Request aborted.~@:>"
                                              :format-arguments '())))))))

    (format *trace-output* "Method: ~S/~S~%Result: ~S Condition: ~S~%"
            method id result condition)

    (when condition          ; TODO (handle-error context condition) ?
      (log:error "~A: ~A" (type-of condition) condition)
      (format *trace-output* "Error: ~A~%" condition)
      (force-output *trace-output*))

    (maphash
     (lambda (uri diagnostics)
       (restart-case
           (handler-bind ((error (lambda (condition)
                                   (log:error "Failed to publish diagnostics ~A" condition)
                                   ())))
             (conn:write-message connection (conn:make-notification
                                             "textDocument/publishDiagnostics"
                                             :uri         uri
                                             :diagnostics (map 'vector #'proto:unparse-diagnostic
                                                               diagnostics))))
         (continue ()
           :report "Skip diagnostics"
           (log:error "failed to publish diagnostics for ~A" uri))))
     diagnostics)

    (cond ((eq result :exit)
           (throw 'exit nil))
          ((not id))
          (method
           (conn:write-message connection (conn:make-response id result))
           #+no (protocol.language-server.connection:write-response
                 connection id result)))))
