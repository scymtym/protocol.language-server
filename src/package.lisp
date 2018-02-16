;;;; package.lisp --- Package definition for the language-server module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; https://github.com/Microsoft/language-server-protocol/

(cl:defpackage #:protocol.language-server
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  ;; Context protocol
  (:export
   #:connection
   #:workspace)

  ;; Workspace protocol
  (:export
   #:root-uri
   #:root-path)

  ;; Document protocol
  (:export
   #:version
   #:text
   #:update
   #:position->index
   #:index->position
   #:word-at)

  ;; Document container protocol
  (:export
   #:document-count
   #:find-document) ; also `setf'

  ;; Dispatch protocol
  (:export
   #:process-method
   #:process-interface-method)

  (:export
   #:language-server))
