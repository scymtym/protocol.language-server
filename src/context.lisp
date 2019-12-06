;;;; context.lisp --- Holds a connection and a workspace.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defclass context ()
  ((%connection          :initarg  :connection
                         :reader   connection)
   ;; Capabilities
   ;; TODO or should capabilities be associated with the workspace
   (%server-capabilities :initarg  :server-capabilities
                         :reader   server-capabilities)
   (%client-capabilities :initarg  :client-capabilities
                         :accessor client-capabilities)
   ;; Workspace
   (%workspace-class     :initarg  :workspace-class
                         :reader   workspace-class
                         :initform 'standard-workspace)
   (%workspace           :reader   workspace
                         :writer   (setf %workspace)
                         :initform nil)))

(defun make-context (connection
                     &rest args &key
                     (class           'context)
                     (capabilities    '())
                     (workspace-class nil workspace-class-supplied?)
                     &allow-other-keys)
  (let ((capabilities (apply #'proto::make-server-capabilities capabilities)))
    (apply #'make-instance class
           :connection          connection
           :server-capabilities capabilities
           (append (when workspace-class-supplied?
                     (list :workspace-class workspace-class))
                   (remove-from-plist
                    args :class :capabilities :workspace-class)))))

(defmethod make-workspace ((context   context)
                           (root-uri  t)
                           (root-path t))
  (make-instance (workspace-class context)
                 :root-uri  root-uri
                 :root-path root-path))
