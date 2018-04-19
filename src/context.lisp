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
                    :initform 'standard-workspace)
   (workspace       :reader   workspace
                    :writer   (setf %workspace)
                    :initform nil)))

;; TODO generic function
;; TODO or should capabilities be associated to the workspace
;; TODO should this get the workspace besides the context?
(defmethod capabilities ((context context))
  '((:text-document-sync          . 2)  ; TODO doesn't seem to help?
    (:hover-provider              . t)
    (:signature-help-provider     . ((:trigger-characters . ("(" " "))))
    (:completion-provider         . ((:resolve-provider   . t)
                                     (:trigger-characters . (":"))))
    (:document-highlight-provider . t)
    (:code-action-provider        . t)
    (:rename-provider             . t)))

(defmethod make-workspace ((context   context)
                           (root-uri  t)
                           (root-path t))
  (make-instance (workspace-class context)
                 :root-uri  root-uri
                 :root-path root-path))
