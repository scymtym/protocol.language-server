;;;; protocol.lisp --- Protocol provided by the methods module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.methods)

;;; Context methods

(defgeneric log-message (message))

(defmethod log-message ((message t))
  (signal (find-symbol "MESSAGE" (find-package "PROTOCOL.LANGUAGE-SERVER")) ; TODO
          :message message))

;;; Workspace methods

;; TODO make a `document-version' or `versioned-document-name' comprising uri and version
(defgeneric did-open (workspace uri version language text))

(defgeneric did-close (workspace document))

(defgeneric did-change-configuration (workspace))

(defgeneric did-change-watched-files (workspace))

(defgeneric symbol-query (workspace query)
  (:documentation
   "Return symbols in WORKSPACE matching QUERY."))

(defgeneric execute-command (workspace command &rest arguments)
  (:method ((workspace t) (command t) &rest arguments)
    (declare (ignore arguments))
    (error "No such command: ~S" command))
  (:documentation
   ""))

(defgeneric edit (workspace edits)
  (:documentation
   ""))

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

(defgeneric signature-help (workspace document position)
  (:documentation
   "Compute and return signature help for POSITION in DOCUMENT."))

(defgeneric definition (workspace document position)
  (:documentation
   ""))

(defgeneric references (workspace document position include-declaration?)
  (:documentation
   ""))

(defgeneric highlight-in-document (workspace document version position)
  (:documentation
   ""))

(defgeneric symbols (workspace document)
  (:documentation
   ""))

(defgeneric code-actions (workspace document range context)
  (:documentation
   ""))

(defgeneric rename (workspace document position new-name)
  (:documentation
   "

    Workspace-wide."))

;;;

(defgeneric publish-diagnostics (document diagnostics))

(defmethod publish-diagnostics (document diagnostics)
  (signal (find-symbol "DIAGNOSTIC" (find-package "PROTOCOL.LANGUAGE-SERVER")) ; TODO
          :diagnostics (alexandria:ensure-list diagnostics)))

;;;
