;;;; package.lisp --- Package definition for tests of the language-server module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.test
  (:use
   #:cl
   #:let-plus

   #:fiveam

   #:protocol.language-server)

  (:export
   #:run-tests))

(cl:in-package #:protocol.language-server.test)

(def-suite :protocol.language-server)

(defun run-tests ()
  (run! :protocol.language-server))
