;;;; package.lisp --- Package definition for the methods module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.methods
  (:use
   #:cl
   #:let-plus)

  ;; Workspace methods
  (:export
   #:did-open
   #:did-close

   #:did-change-configuration
   #:did-change-watched-files

   #:edit

   #:execute-command)

  ;; Document methods
  (:export
   #:will-save
   #:did-save
   #:did-change

   #:completion
   #:hover
   #:definition
   #:references
   #:highlight-in-document
   #:symbols

   #:code-actions
   #:rename)

  ;; Experimental
  (:export
   #:publish-diagnostics))
