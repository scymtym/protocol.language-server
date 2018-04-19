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

(defun process-request (connection context)
  (let+ (((&values id method arguments)
          (protocol.language-server.connection:read-request connection))
         (diagnostics (make-hash-table :test #'eq))
         ((&values result condition)
          (restart-case
              (block nil
                (handler-bind
                    ((error (lambda (condition)
                              (if *debug*
                                  (invoke-debugger condition)
                                  (return (values nil condition)))))
                     (diagnostic (lambda (condition) ; TODO generalize to all notifications
                                   (appendf (gethash (uri condition) diagnostics '())
                                            (diagnostics condition)))))
                  (apply #'process-method context method arguments)))
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
       (protocol.language-server.connection:write-notification
        connection "textDocument/publishDiagnostics"
        `((:uri         . ,uri)
          (:diagnostics . ,(map 'vector #'protocol.language-server.protocol:unparse-diagnostic
                                diagnostics)))))
     diagnostics)

    (cond
      ((eq result :exit)
       (throw 'exit nil))
      ((not id))
      (t
       (protocol.language-server.connection:write-response
        connection id result)))))
