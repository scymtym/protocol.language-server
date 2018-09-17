;;;; package.lisp --- Package definition for the transport module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.transport
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  (:export
   #:read-message
   #:write-message)

  (:documentation
   "Functions for sending and receiving JSONRPC messages."))
