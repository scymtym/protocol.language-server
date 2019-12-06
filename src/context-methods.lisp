;;;; context-methods.lisp --- Methods and dispatch for context.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defmethod process-method ((object context)
                           (method (eql :initialize))
                           &key
                           process-id
                           root-uri
                           root-path
                           capabilities
                           initialization-options)
  ;; Create and initialize workspace.
  (setf (%workspace object)          (make-workspace object #|later process-id|# root-uri root-path
                                        ; later capabilities initialization-options
                                                     )
        (client-capabilities object) (proto:parse capabilities 'proto::client-capabilities))
  ;; Compute capabilities and send as reply.
  `((:capabilities . ,(proto:unparse (server-capabilities object)))))

;; TODO what is this supposed to do?
(defmethod process-method ((object context) (method (eql :initialized)) &key))

;; TODO what is this supposed to do?
(defmethod process-method ((object context) (method (eql :shutdown)) &key))

(defmethod process-method ((object context) (method (eql :exit)) &key)
  :exit)

;;; Dispatching to workspace, etc.

(defmethod process-method ((object context) (method string)
                           &rest args &key)
  (let+ ((index (position #\/ method))
         ((&flet as-keyword (string)
            (find-symbol (string-upcase string) (find-package '#:keyword))))
         ((&values method/symbol interface/symbol)
          (if index
              (values (as-keyword (subseq method (1+ index)))
                      (as-keyword (subseq method 0 index)))
              (values (as-keyword method)))))
    (cond ((eq interface/symbol :$) ; TODO used for cancel?
           (error "No implemented"))
          ((and interface/symbol method/symbol)
           (apply #'process-interface-method
                  object interface/symbol method/symbol args))
          (method/symbol
           (apply #'process-method object method/symbol args))
          (t
           (error "No such method: \"~A\"." method)))))

(defmethod process-interface-method ((object    context)
                                     (interface t)
                                     (method    t)
                                     &rest args &key)
  (unless (workspace object)
    (error "~@<Workspace not yet initialized for ~A.~@:>" object))
  (apply #'process-interface-method (workspace object) interface method args))

(defmethod process-interface-method ((object    context)
                                     (interface (eql :completionitem))
                                     (method    (eql :resolve))
                                     &rest item)
  (plist-alist item))
