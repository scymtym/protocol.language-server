;;;; document-methods.lisp --- Adapters for methods of the TextDocument interface.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
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
  ;; TODO make sure to delay any parsing and such until all changes have been applied
  (let+ (((&flet apply-change (change)
            (let+ (((text range &ign)
                    (proto:parse-text-document-content-change change)))
              (update object range text)))))
    (map nil #'apply-change content-changes) ; TODO this should just be the default method
    (setf (%version object) version)))

(defmethod process-method ((object document)
                           (method (eql :completion))
                           &key
                           position)
  (let+ ((position (text.source-location::attach-text
                    (proto:parse position 'position) (text object)))
         ((&values items incomplete?)
          (methods:completion *workspace* object position))
         ((&flet unparse-item (item)
            ;; If the implementation used a string instead of
            ;; explicitly specifying the range, fill in a default
            ;; range here.
            (let ((edit (proto::edit item)))
              (when (stringp edit)
                (let ((range (text.source-location:make-range
                              position position)))
                  (setf (proto::%edit item)
                        (proto:make-edit range edit)))))
            (proto:unparse-completion-item item))))
    `((:items         . ,(map 'vector #'unparse-item items))
      (:is-incomplete . ,(if incomplete? t proto::+false+)))))

(defmethod process-method ((object document)
                           (method (eql :hover))
                           &key
                           position)
  (let* ((position (text.source-location::attach-text
                    (proto:parse position 'position) (text object)))
         (result   (methods:hover *workspace* object position)))
    (if result
        (proto:unparse-hover-result result)
        (make-hash-table)))) ; TODO hack for Emacs' lsp-mode but null is a legal response

(defmethod process-method ((object document)
                           (method (eql :signaturehelp))
                           &key
                           position)
  (let* ((position (text.source-location::attach-text
                    (proto:parse position 'position) (text object)))
         (result   (methods:signature-help *workspace* object position)))
    (when result
      (proto:unparse result))))

(defmethod process-method ((object document)
                           (method (eql :definition))
                           &key
                           position)
  (let* ((position (text.source-location::attach-text
                    (proto:parse position 'position) (text object)))
         (result   (methods:definition *workspace* object position)))
    (typecase result
      (null nil)
      (list (map 'vector #'proto:unparse result))
      (t    (proto:unparse result)))))

(defmethod process-method ((object document)
                           (method (eql :references))
                           &key
                           position
                           include-declaration)
  (let ((position (text.source-location::attach-text
                   (proto:parse position 'position) (text object))))
    (map 'vector #'proto:unparse
         (methods:references *workspace* object position include-declaration))))

(defmethod process-method ((object document)
                           (method (eql :documenthighlight))
                           &key
                           version
                           position)
  (let* ((position   (text.source-location::attach-text
                      (proto:parse position 'position) (text object)))
         (highlights (methods:highlight-in-document
                      *workspace* object version position)))
    (map 'vector #'proto:unparse-highlight highlights)))

(defmethod process-method ((object document)
                           (method (eql :documentsymbol))
                           &key)
  ;; symbolinformation array or null
  (map 'vector #'proto:unparse (methods:symbols *workspace* object)))

(defmethod process-method ((object document)
                           (method (eql :codeaction))
                           &key
                           range
                           context)
  (let* ((range   (text.source-location::attach-text
                   (proto:parse range 'text.source-location:range) (text object)))
         (actions (methods:code-actions *workspace* object range context)))
    (map 'vector #'proto::unparse actions)))

(defmethod process-method ((object document)
                           (method (eql :rename))
                           &key
                           position
                           new-name)
  (let* ((position (text.source-location::attach-text
                    (proto:parse position 'position) (text object)))
         #+TODO-later (edits    (protocol.language-server.methods:rename
                    nil object position new-name)))
    ;; /workspace-wide/ rename
    (methods:rename nil object position new-name)
    #+TODO-later `((:document-changes . ,(map 'vector (compose #'protocol.language-server.protocol::unparse-text-document-edit
                                                  (lambda (edit)
                                                    (protocol.language-server.protocol::make-text-document-edit
                                                     *uri* *version*
                                                     edits)))
                                 (list document))))))
