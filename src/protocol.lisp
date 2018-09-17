;;;; protocol.lisp --- Protocol provided by the language-server module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

;;; Context protocol

(defgeneric connection (context)
  (:documentation
   "TODO"))

(defgeneric capabilities (context)
  (:documentation
   "Return capabilities of CONTEXT."))

(defgeneric workspace (context)
  (:documentation
   "TODO"))

;;; Workspace protocol

(defgeneric root-uri (workspace)
  (:documentation
   "TODO"))

(defgeneric root-path (workspace)
  (:documentation
   "TODO"))

;;; Document protocol

(defgeneric version (document)
  (:documentation
   "TODO"))

(defgeneric text (document)
  (:documentation
   "TODO"))

(defgeneric update (document start-index end-index new-text)
  (:documentation
   "TODO"))

(defgeneric position->index (document line character)
  (:documentation
   "TODO"))

(defgeneric index->position (document index)
  (:documentation
   "TODO"))

(defgeneric word-at (document position)
  (:documentation
   "TODO"))

;;;  Document container protocol

(defgeneric document-count (container)
  (:documentation
   "TODO"))

(defgeneric find-document (uri container)
  (:documentation
   "TODO"))

(defgeneric (setf find-document) (new-value uri container)
  (:documentation
   "TODO"))

;;; Method dispatch protocol

;; TODO renamed to handle-call? what about notifications?
(defgeneric process-method (object method &key &allow-other-keys))

(defgeneric process-interface-method (object interface method &key &allow-other-keys))
