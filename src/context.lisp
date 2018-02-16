;;;; context.lisp --- Holds a connection and a workspace.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defclass context ()
  ((connection      :initarg  :connection
                    :reader   connection)
   (workspace-class :initarg  :workspace-class
                    :reader   workspace-class
                    :initform 'workspace)
   (workspace       :reader   workspace
                    :writer   (setf %workspace)
                    :initform nil)))

(defmethod make-workspace ((context   context)
                           (root-uri  t)
                           (root-path t))
  (make-instance (workspace-class context)
                 :root-uri  root-uri
                 :root-path root-path))
