;;;; connection.lisp --- Connection class, reading/writing messages.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.connection)

(macrolet ((define (name predicate)
             `(defun ,name (thing)
                (when (streamp thing) (,predicate thing)))))
  (define %input-stream-p  input-stream-p)
  (define %output-stream-p output-stream-p))

(defclass connection ()
  ((%input  :initarg  :input
            :type     (and stream (satisfies %input-stream-p))
            :reader   input)
   (%output :initarg  :output
            :type     (and stream (satisfies %output-stream-p))
            :reader   output))
  (:default-initargs
   :input  (cl:error "missing initarg :input")
   :output (cl:error "missing initarg :output")))

(defun make-connection (input output &key (class 'connection))
  (make-instance class :input input :output output))

(defun submit-to-visual-analyzer (direction message)
  (with-simple-restart (continue "~@<Skip this shit.~@:>")
    (when (find-package '#:protocol.language-server.visual-analyzer)
      (handler-bind
          ((error (lambda (condition)
                    (log:error "Error submitting ~A message to visual analyzer: ~A"
                               direction condition)
                    (let ((backtrace (with-output-to-string (stream)
                                       (sb-debug:print-backtrace :stream stream))))
                      (log:error backtrace))
                    (continue))))
        (uiop:symbol-call '#:protocol.language-server.visual-analyzer '#:add-message direction message)))))

(defmethod read-message ((connection connection))
  (let* ((raw       (transport:read-message (input connection)))
         (request   (json:decode-json-from-string raw))

         (id        (assoc-value request :id))
         (method    (assoc-value request :method))
         (arguments (assoc-value request :params))
         (result    (assoc-value request :result)))
    (log:info "~@<=> ~:[     Notification~;~:*[~D] Request~] ~A~@:_~
               ~2@T~@<~/protocol.language-server.connection::print-maybe-alist/~:>~:>"
              id method arguments)
    (let ((message (cond ((and method id)
                          (apply #'make-request id method
                                 (alist-plist arguments)))
                         (id
                          (make-response id result))
                         (t
                          (apply #'make-notification method
                                 (alist-plist arguments))))))
      (ignore-errors (submit-to-visual-analyzer :client->server message))
      (values message id method (alist-plist arguments)))))

(defmethod write-message ((connection connection) (message t))
  (ignore-errors (submit-to-visual-analyzer :server->client message))
  (let ((raw (json:encode-json-to-string (to-alist message))))
    (transport:write-message (output connection) raw)))
