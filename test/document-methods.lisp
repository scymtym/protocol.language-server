;;;; document-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server.test)

(in-suite :protocol.language-server)

;;; Deleting first line
(let ((d (make-instance 'document
                        :language :foo
                        :version 1
                        :text "variables:
foo")))
  (process-method d :didchange :content-changes '(((:RANGE (:START (:LINE . 0) (:CHARACTER . 0))
                                                    (:END (:LINE . 0) (:CHARACTER . 10)))
                                                   (:RANGE-LENGTH . 10) (:TEXT . ""))))
  (process-method d :didchange :content-changes '(((:RANGE (:START (:LINE . 1) (:CHARACTER . 0))
                                                    (:END  (:LINE . 1) (:CHARACTER . 0)))
                                                   (:RANGE-LENGTH . 0)
                                                   (:TEXT . "a"))))
  (text d))
