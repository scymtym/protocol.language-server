;;;; package.lisp --- Package definition for the connection module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.connection
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:shadow
   #:error
   #:method)

  (:local-nicknames
   (#:transport #:protocol.language-server.transport))

  ;; Connection protocol
  (:export
   #:read-request
   #:write-response
   #:write-notification)

  ;; Connection construction
  (:export
   #:make-connection))
