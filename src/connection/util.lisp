;;;; util.lisp --- Utilities used in the connection module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.connection)

(defun print-maybe-alist (stream object &optional colon? at?)
  (declare (ignore colon? at?))
  (typecase object
    ((or null (cons keyword list))
     (format stream "~{~16A ~:S~^~@:_~}" object))
    ((or null (cons (cons (keyword)) list))
     (format stream "~{~16A ~:S~^~@:_~}" (alist-plist object)))
    (t
     (format stream "~:S" object))))
