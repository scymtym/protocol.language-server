(cl:in-package #:protocol.language-server.connection)

(defun print-maybe-alist (stream object &optional colon? at?)
  (declare (ignore colon? at?))
  (typecase object
    ((or null (cons keyword list))
     (format stream "~{~16A ~:S~^~@:_~}" object))
    ((or null (cons (cons (keyword)) list))
     (format stream "~{~16A ~:S~^~@:_~}" (alist-plist object)))
    (t
     (format stream "~:S" object))))
