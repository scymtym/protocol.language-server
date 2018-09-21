(cl:defpackage #:protocol.language-server.contributor
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:proto   #:protocol.language-server.protocol)
   (#:methods #:protocol.language-server.methods))

  ;;  Context contributor protocol
  (:export
   #:contexts
   #:contexts-using-contributors
   #:context-contributions

   #:make-context-contributors)

  ;; Hover contributor protocol
  (:export
   #:hover-using-contributors
   #:hover-contribution

   #:make-hover-contributors)

  ;; Completion contributor protocol
  (:export
   #:completions-using-contributors
   #:completion-contributions

   #:make-completion-contributors)

  ;; Mixins
  (:export
   #:context-contributors-mixin
   #:context-contributors

   #:hover-contributors-mixin
   #:hover-contributors

   #:completion-contributors-mixin
   #:completion-contributors))
