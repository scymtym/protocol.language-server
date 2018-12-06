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
   #:backtrace
   #:error
   #:method)

  (:local-nicknames
   (#:transport #:protocol.language-server.transport))

  ;; Messages
  (:export
   #:error
   #:code
   #:message
   #:backtrace

   #:call-message
   #:method
   #:arguments

   #:make-request

   #:notification

   #:make-notification

   #:id-message
   #:id

   #:request

   #:response
   #:result

   #:make-response)

  ;; Connection protocol
  (:export
   #:read-message
   #:write-message)

  ;; Connection construction
  (:export
   #:make-connection)

  ;; Deprecated
  (:export
   #:read-request
   #:write-response
   #:write-notification))
