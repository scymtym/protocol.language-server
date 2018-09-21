;;;; workspace-methods.lisp --- Adapters for methods of the Workspace interface.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

;;; Dispatch to workspace

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :workspace))
                                     (method    t)
                                     &rest args &key)
  (apply #'process-method object method args))

;;; Special document-related methods and dispatch to document

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    (eql :didopen))
                                     &key
                                     text-document)
  (let+ (((&values uri version) (proto:parse-text-document text-document))
         (language-id (assoc-value text-document :language-id))
         (language    (make-keyword (string-upcase language-id)))
         (text        (assoc-value text-document :text)))
    (log:info "new document" uri language version text)
    (handler-bind ((diagnostic (lambda (condition) ; TODO hack
                                 (setf (%uri condition) uri))))
      (setf (find-document uri object)
            (make-document object language version text)))))

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    (eql :didclose))
                                     &key
                                     text-document)
  (let ((uri (proto:parse-text-document text-document)))
    (setf (find-document uri object) nil)))

(defvar *workspace*)

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    t)
                                     &rest args &key text-document)
  (let+ (((&values uri version) (proto:parse-text-document text-document))
         (document (find-document uri object)))
    (handler-bind ((diagnostic (lambda (condition)
                                 (setf (%uri condition) uri))))
      (let ((*workspace* object))
        (apply #'process-method document method
               :version version (remove-from-plist args :text-document))))))

;;; Workspace methods

(defmethod process-method ((object workspace)
                           (method (eql :didchangeconfiguration))
                           &key))

(defmethod process-method ((object workspace)
                           (method (eql :didchangewatchedfiles))
                           &key))

(defmethod process-method ((object workspace)
                           (method (eql :symbol))
                           &key
                           query)
  ;; symbol-information array or null
  )

(defmethod process-method ((object workspace)
                           (method (eql :executecommand))
                           &key
                           command
                           arguments)
  (log:info "~@<~A is executing command ~S ~S~@:>" object command arguments)
  (if-let ((command (find-symbol (string-upcase command)
                                 (find-package '#:keyword))))
    (apply #'methods:execute-command object command arguments)
    (error "No such command: ~A" command)))
