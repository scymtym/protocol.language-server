;;;; protocol.language-server.visual-analyzer.asd --- System definition for the visual analyzer.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "protocol.language-server.visual-analyzer"
  :description "A visual protocol analyzer for language server protocol sessions"
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                 "0.2")

                "text.source-location.print" ; for snippet

                "mcclim"
                "clouseau"

                (:version "protocol.language-server" (:read-file-form "version-string.sexp")))

  :components  ((:module     "visual-analyzer"
                 :pathname   "src/visual-analyzer"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "model")

                              (:file       "view")
                              (:file       "snippet")

                              (:file       "application"))))

  ; :in-order-to  ((test-op (test-op "protocol.language-server.visual-analyzer/test")))
  )

#+later (defsystem "protocol.language-server.visual-analyzer/test"
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
