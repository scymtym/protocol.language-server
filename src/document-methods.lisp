;;;; document-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defmethod process-method ((object document)
                           (method (eql :willsave))
                           &key
                             ))

(defmethod process-method ((object document)
                           (method (eql :didsave))
                           &key
                             ))

(defmethod process-method ((object document)
                           (method (eql :didchange))
                           &key
                           content-changes
                           version)
  (let+ (((&flet apply-change (change)
            (let+ (((text range &ign)
                    (parse-text-document-content-change change)))
              (update* object range text)))))
    (map nil #'apply-change content-changes)
    (setf (%version object) version)))

(defmethod process-method ((object document)
                           (method (eql :completion))
                           &key
                           position)
  (let* ((text (word-at object (multiple-value-call #'cons (parse-position position)))))
    ))

(defmethod process-method ((object document)
                           (method (eql :hover))
                           &key
                           position)
  (let+ (((&values line column) (parse-position position))
         (word (word-at object (cons line column)))) ; TODO return range
    (log:info word line column)
    ))

(defmethod process-method ((object document)
                           (method (eql :definition))
                           &key
                           position)
  (let+ (((&values line column) (parse-position position)))
    ))

(defmethod process-method ((object document)
                           (method (eql :references))
                           &key
                           position
                           include-declaration)
  (let+ (((&values line column) (parse-position position)))
    ))

(defmethod process-method ((object document)
                           (method (eql :documenthighlight))
                           &key
                           version
                           position)
  (let+ (((&values line column) (parse-position position)))
    ))

(defmethod process-method ((object document)
                           (method (eql :documentsymbol))
                           &key)
  ; symbolinformation array or null
  )

(defmethod process-method ((object document)
                           (method (eql :rename))
                           &key
                           position
                           new-name)
  (let+ (((&values line column) (parse-position position)))
    ;; /workspace-wide/ rename
    ))
