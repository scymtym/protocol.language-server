;;;; workspace.lisp --- Root path and a collection of documents.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

;;; `workspace'

(defclass workspace ()
  ())

;;; `document-container-mixin'

(defclass document-container-mixin ()
  ((documents :reader   %documents
              :initform (make-hash-table :test #'equal))))

(defmethod print-items:print-items append ((object document-container-mixin))
  `((:document-count ,(document-count object) " ~D document~:P")))

(defmethod document-count ((container document-container-mixin))
  (hash-table-count (%documents container)))

(defmethod find-document ((uri string) (container document-container-mixin))
  (gethash uri (%documents container)))

(defmethod (setf find-document) ((new-value t)
                                 (uri       string)
                                 (container document-container-mixin))
  (setf (gethash uri (%documents container)) new-value))

(defmethod (setf find-document) ((new-value (eql nil))
                                 (uri       string)
                                 (container document-container-mixin))
  (remhash uri (%documents container)))

;;; `document-class-mixin'

(defclass document-class-mixin ()
  ((document-class :initarg  :document-class
                   :reader   document-class
                   :initform 'document)))

(defmethod make-document ((container document-class-mixin)
                          (language  t)
                          (version   t)
                          (text      t))
  (make-instance (document-class container)
                 :language language
                 :version  version
                 :text     text))

;;; `standard-workpace'

(defclass standard-workspace (workspace
                              document-container-mixin
                              document-class-mixin
                              print-items:print-items-mixin)
  ((root-uri  :initarg  :root-uri
              :reader   root-uri)
   (root-path :initarg  :root-path
              :reader   root-path))
  (:default-initargs
   :root-uri  (error "missing required initarg :root-uri")
   :root-path (error "missing required initarg :root-path")))

(defmethod print-items:print-items append ((object standard-workspace))
  `((:root-uri ,(root-uri object) "~A" ((:before :document-count)))))
