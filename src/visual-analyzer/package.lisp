(cl:defpackage #:protocol.language-server.visual-analyzer
  (:use
   #:clim-lisp
   #:let-plus
   #:alexandria)

  (:local-nicknames
   (#:sloc  #:text.source-location)

   (#:lsp   #:protocol.language-server)
   (#:con   #:protocol.language-server.connection)
   (#:proto #:protocol.language-server.protocol))

  (:shadow
   #:trace
   #:inspect)
  (:export
   #:print-annotated-lines-using-style))
