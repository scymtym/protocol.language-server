;;;; protocol.lisp --- Protocol provided by the methods module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.methods)

;;; Document methods

(defgeneric will-save (workspace document)
  (:documentation
   ""))

(defgeneric did-save (workspace document)
  (:documentation
   ""))

(defgeneric did-change (workspace document version content-changes)
  (:documentation
   ""))

(defgeneric completion (workspace document position)
  (:documentation
   ""))

(defgeneric hover (workspace document position)
  (:documentation
   ""))

(defgeneric definition (workspace document position)
  (:documentation
   ""))

(defgeneric references (workspace document position include-declaration?)
  (:documentation
   ""))

(defgeneric highlight-TODO (workspace document version position)
  (:documentation
   ""))

(defgeneric symbols (workspace document)
  (:documentation
   ""))

(defgeneric rename (workspace document position new-name)
  (:documentation
   "

Workspace-wide."))
