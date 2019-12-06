;;;; protocol.lisp --- Protocol provided by the language-server module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

;;; Context protocol

(defgeneric connection (context)
  (:documentation
   "Return the connection used by CONTEXT."))

(defgeneric capabilities (context)
  (:documentation
   "Return capabilities of CONTEXT."))

(defgeneric workspace (context)
  (:documentation
   "Return the workspace of CONTEXT.

    This function must not be called before CONTEXT has processed the
    \"initialize\" method call."))

(defgeneric make-workspace (context root-uri root-path)
  (:documentation
   "Make and return a workspace for CONTEXT, ROOT-URI and ROOT-PATH."))

;;; Workspace protocol

(defgeneric root-uri (workspace)
  (:documentation
   "Return the root URI of WORKSPACE."))

;;; Document protocol

(defgeneric version (document)
  (:documentation
   "Return the current version of DOCUMENT."))

(defgeneric text (document)
  (:documentation
   "Return the current text of DCUMENT."))

(defgeneric update (document range new-text)
  (:documentation
   "Replace RANGE in DOCUMENT with NEW-TEXT.

    If RANGE is `nil', replace the entire text of DOCUMENT."))

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
   "Return the document associated to URI in CONTAINER."))

(defgeneric (setf find-document) (new-value uri container)
  (:documentation
   "Associate the document NEW-VALUE to URI in CONTAINER."))

(defgeneric note-adopted (container document)
  (:method-combination progn)
  (:method progn ((container t) (document t)))
  (:documentation
   "Called when DOCUMENT is added to CONTAINER"))

;;; Method dispatch protocol

;; TODO renamed to handle-call? what about notifications?
(defgeneric process-method (object method &key &allow-other-keys))

(defgeneric process-interface-method (object interface method &key &allow-other-keys))
