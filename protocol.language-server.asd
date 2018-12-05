;;;; protocol.language-server.asd --- System definition for the main system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "protocol.language-server"
  :description "An implementation of the language server protocol."
  :license     "LLGPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "split-sequence"
                (:version "let-plus"              "0.2")
                (:version "utilities.print-items" "0.1")

                (:version "log4cl"                "1.1")

                "puri"
                (:version "text.source-location"  "0.1")

                (:version "cl-json"               "0.5"))

  :components  ((:module     "src"
                 :depends-on ("connection"
                              "protocol"
                              "methods")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "context")
                              (:file       "context-methods")

                              (:file       "workspace")
                              (:file       "workspace-methods")

                              (:file       "document")
                              (:file       "document-methods")

                              (:file       "language-server")))

                (:module     "transport"
                 :pathname   "src/transport"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "transport")))

                (:module     "connection"
                 :pathname   "src/connection"
                 :depends-on ("transport")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "messages")
                              (:file       "connection")))

                (:module     "protocol"
                 :pathname   "src/protocol"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "util")
                              (:file       "messages")))

                (:module     "methods"
                 :pathname   "src/methods"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol"))))

  :in-order-to  ((test-op (test-op "protocol.language-server/test"))))

(defsystem "protocol.language-server/test"
  :description "Tests for the protocol.language-server system."
  :license     "LLGPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "fiveam"                   "1.3")

                (:version "protocol.language-server" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")

                              (:file "workspace"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:protocol.language-server.test '#:run-tests)))
