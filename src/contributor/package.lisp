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
   #:diagnostic ; name of the aspect

   #:diagnostics
   #:diagnostics-using-contributors
   #:diagnostics-contributions)

  ;; Context contributor protocol
  (:export
   #:context ; name of the aspect

   #:contexts
   #:contexts-using-contributors
   #:context-contributions)

  ;; Hover contributor protocol
  (:export
   #:hover ; name of the aspect

   #:hover-using-contributors
   #:hover-contribution)

  ;; Signature contributor protocol
  (:export
   #:signature ; name of the aspect

   #:signatures-using-contributors
   #:signature-contributions)

  ;; Document highlight contributor protocol
  (:export
   #:document-highlight ; name of the aspect

   #:document-highlight-using-contributors
   #:document-highlight-contributions)

  ;; Completion contributor protocol
  (:export
   #:completion

   #:completions-using-contributors
   #:completion-contributions)

  ;; Definition contributor protocol
  (:export
   #:definition ; name of the aspect

   #:definitions-using-contributors
   #:definition-contributions)

  ;; Reference contributor protocol
  (:export
   #:reference ; name of the aspect

   #:references-using-contributors
   #:reference-contributions)

  ;; Contributor creation protocol
  (:export
   #:make-contributors)

  ;; Mixins
  (:export
   #:diagnostics-contributors-mixin
   #:diagnostics-contributors

   #:context-contributors-mixin
   #:context-contributors

   #:hover-contributors-mixin
   #:hover-contributors

   #:signature-contributors-mixin
   #:signature-contributors

   #:document-highlight-contributors-mixin
   #:document-highlight-contributors

   #:completion-contributors-mixin
   #:completion-contributors

   #:definition-contributors-mixin
   #:definition-contributors

   #:reference-contributors-mixin
   #:reference-contributors))
