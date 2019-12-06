;;;; package.lisp --- Package definition for the language-server module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; https://github.com/Microsoft/language-server-protocol/

(cl:defpackage #:protocol.language-server
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  (:local-nicknames
   (#:conn    #:protocol.language-server.connection)
   (#:proto   #:protocol.language-server.protocol)
   (#:methods #:protocol.language-server.methods))

  ;; Context protocol
  (:export
   #:connection
   #:capabilities
   #:workspace
   #:make-workspace

   #:context
   #:make-context)

  ;; Workspace protocol and class
  (:export
   #:root-uri
   #:root-directory

   #:workspace
   #:standard-workspace)

  ;; Document protocol and class
  (:export
   #:workspace

   #:language
   #:version
   #:text
   #:update
   #:position->index
   #:index->position
   #:word-at

   #:document)

  ;; Document container protocol
  (:export
   #:document-count
   #:find-document ; also `setf'
   #:note-adopted

   #:document-container-mixin)

  ;; Document class and creation protocol and mixin
  (:export
   #:document-class
   #:make-document

   #:document-class-mixin)

  ;; Root URI mixin
  (:export
   #:root-uri-mixin)

  ;; Dispatch protocol
  (:export
   #:process-method
   #:process-interface-method)

  ;; Standard workspace
  (:export
   #:standard-workspace)

  (:export
   #:language-server
   #:documents/alist))
