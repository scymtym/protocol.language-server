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

(defmethod documents/alist ((container document-container-mixin))
  (hash-table-alist (%documents container)))

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

;;; `root-uri-mixin'

(defclass root-uri-mixin ()
  ((%root-uri :type     puri:uri
              :reader   root-uri
              :accessor %root-uri)))

(defmethod initialize-instance :before ((instance root-uri-mixin)
                                        &key
                                        (root-uri  nil root-uri-supplied?)
                                        (root-path nil root-path-supplied?))
  (declare (ignore root-uri root-path))
  (unless (or root-uri-supplied? root-path-supplied?)
    (error "At least one of ~S and ~S must be supplied."
           :root-uri :root-path)))

(defmethod shared-initialize :after ((instance   root-uri-mixin)
                                     (slot-naems t)
                                     &key
                                     (root-uri  nil root-uri-supplied?)
                                     (root-path nil root-path-supplied?))
  (flet ((ensure-directory-path (uri)
           (puri:copy-uri
            uri :path (namestring (uiop:ensure-directory-pathname
                                   (puri:uri-path uri))))))
    (cond (root-uri-supplied?
           (setf (%root-uri instance)
                 (ensure-directory-path (puri:uri root-uri))))
          (root-path-supplied?
           (setf (%root-uri instance)
                 (ensure-directory-path
                  (make-instance 'puri:uri
                                 :scheme :file
                                 :path   (namestring root-path))))))))

(defmethod print-items:print-items append ((object root-uri-mixin))
  `((:root-uri ,(root-uri object) "~A" ((:before :document-count)))))

(defmethod root-directory ((workspace root-uri-mixin))
  (let ((uri (root-uri workspace)))
    (assert (eq (puri:uri-scheme uri) :file))
    (pathname (puri:uri-path uri))))

(defmethod root-path ((workspace root-uri-mixin))
  (namestring (root-directory workspace)))

(declaim (sb-ext:deprecated :early ("protocol.language-server" "0.1")
                            (function root-path :replacement root-directory)))

;;; `standard-workpace'

(defclass standard-workspace (workspace
                              document-container-mixin
                              document-class-mixin
                              root-uri-mixin
                              print-items:print-items-mixin)
  ())
