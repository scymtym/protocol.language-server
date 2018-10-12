(cl:defpackage #:protocol.language-server.contributor
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:proto   #:protocol.language-server.protocol)
   (#:methods #:protocol.language-server.methods))

  ;; Diagnostics contributor protocol
  (:export
   #:diagnostics
   #:diagnostics-using-contributors
   #:diagnostics-contributions

   #:make-diagnostics-contributors)

  ;; Context contributor protocol
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
   #:diagnostics-contributors-mixin
   #:diagnostics-contributors

   #:context-contributors-mixin
   #:context-contributors

   #:hover-contributors-mixin
   #:hover-contributors

   #:completion-contributors-mixin
   #:completion-contributors))
