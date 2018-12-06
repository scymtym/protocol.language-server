(cl:in-package #:protocol.language-server.connection.test)

(def-suite* :protocol.language.server.connection.messages
  :in :protocol.language-server.connection)

(test error.print

  (is-false (emptyp (princ-to-string (make-error 2 "bla bla")))))

(test request.print

  (is-false (emptyp (princ-to-string (make-request 6 "textDocument/hover")))))

(test notification.to-alist

  (finishes (to-alist (make-notification "textDocument/hover"))))
