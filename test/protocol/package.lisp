(cl:defpackage #:protocol.language-server.protocol.test
  (:use
   #:cl

   #:fiveam

   #:protocol.language-server.protocol))

(cl:in-package #:protocol.language-server.protocol.test)

(def-suite :protocol.language-server.protocol)
