;;;; document-methods.lisp --- Unit tests for RPC methods related to documents.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.test)

(in-suite :protocol.language-server)

;;; Deleting first line
(test didchange/smoke
  "Smoke test for the implementation of the didChange method."

  (let ((document (make-instance 'document
                                 :language :foo
                                 :version  1
                                 :text     "variables:
foo")))
    (process-method document :didchange :content-changes '(((:RANGE (:START (:LINE . 0) (:CHARACTER . 0))
                                                             (:END (:LINE . 0) (:CHARACTER . 10)))
                                                            (:RANGE-LENGTH . 10) (:TEXT . ""))))
    (process-method document :didchange :content-changes '(((:RANGE (:START (:LINE . 1) (:CHARACTER . 0))
                                                             (:END  (:LINE . 1) (:CHARACTER . 0)))
                                                            (:RANGE-LENGTH . 0)
                                                            (:TEXT . "a"))))
    (text document)))
