(cl:defpackage #:protocol.language-server.connection.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:protocol.language-server.connection)

  (:shadowing-import-from #:protocol.language-server.connection
   #:backtrace
   #:error
   #:method))

(cl:in-package #:protocol.language-server.connection.test)

(def-suite :protocol.language-server.connection
  :in :protocol.language-server)
