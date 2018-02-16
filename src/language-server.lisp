;;;; language-server.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defun language-server (input output)
  (catch 'exit
    (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
      (loop :with connection = (make-instance 'connection :input input :output output)
         :with context = (make-instance 'context :connection connection)
         :do (process-request connection context)))))

(defun process-request (connection context)
  (let+ (((&values id method arguments) (read-request connection))
         ((&values result condition)
          (ignore-errors (apply #'process-method context method arguments))))
    (when condition
      (log:error "~A: ~A" (type-of condition) condition)
      (format *trace-output* "Error: ~A~%" condition)
      (force-output *trace-output*))

    ; (print request *trace-output*)
    ; (print response *trace-output*)
    ; (force-output *trace-output*)

    (cond
      ((eq result :exit)
       (throw 'exit nil))
      ((not id))
      (t
       (write-response connection id result)))))
