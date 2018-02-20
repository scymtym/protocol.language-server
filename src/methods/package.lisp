;;;; package.lisp --- Package definition for the methods module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.methods
  (:use
   #:cl)

  ;; Document methods
  (:export
   #:will-save
   #:did-save
   #:did-change

   #:completion
   #:hover
   #:definition
   #:references
   #:highlight-TODO
   #:symbols

   #:rename))
