;;;; protocol.language-server.contributor.asd --- System definition for contributors.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "protocol.language-server.contributor"
  :description "A contributor-based framework for providing LSP features."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                 "0.2")

                (:version "text.source-location"     "0.1")

                (:version "protocol.language-server" (:read-file-form "version-string.sexp")))

  :components  ((:module     "contributor"
                 :pathname   "src/contributor"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "mixins"))))

  ; :in-order-to  ((test-op (test-op "protocol.language-server/test")))
  )

#+later (defsystem "protocol.language-server.contributor/test"
  :description "Tests for the protocol.language-server.contributor system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "fiveam"                               "1.3")

                (:version "protocol.language-server.contributor" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:protocol.language-server.test '#:run-tests)))
