;;;; protocol.lisp --- Protocol provided by the protocol module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.protocol)

(defgeneric parse (data message-class)
  (:documentation
   "Parse the JSON-based representation DATA into an instance of MESSAGE-CLASS."))

(defgeneric unparse (message)
  (:documentation
   "Unparse MESSAGE into a JSON-based representation."))
