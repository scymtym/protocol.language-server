(defsystem "protocol.language-server"
  :description "An implementation of the language server protocol."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ()

  :components  ((:module     "src"
                 :serial     t
                 :components ()))

  :in-order-to  ((test-op (test-op "protocol.language-server/test"))))

(defsystem "protocol.language-server/test"
  :description "Tests for the protocol.language-server system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "protocol.language-server" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ()))

  :perform     (test-op (operation component)
                 ))
