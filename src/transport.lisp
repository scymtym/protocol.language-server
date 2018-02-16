;;;; transport.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocol.language-server)

(defconstant +cr+ 13)

(defconstant +lf+ 10)

(define-constant +crlf*2+
    (coerce (list +cr+ +lf+ +cr+ +lf+) '(vector (unsigned-byte 8)))
  :test #'equalp)

(defun parse-header-fields (fields)
  (loop :for field :in fields
     :collect (let+ ((string (sb-ext:octets-to-string field :external-format :ascii))
                     ((name value) (split-sequence #\: string)))
                (cons name (string-left-trim '(#\Space) value)))))

;; TODO separate package?
(defun transport/read-request (stream)
  (prog ((header-fields '())
         (header-field  (make-array 0
                                    :element-type '(unsigned-byte 8)
                                    :adjustable   t
                                    :fill-pointer 0))
         (unread        '()))
   :header-start-of-line
     (setf unread (nreverse unread))
     (replace header-field unread)
     (setf (fill-pointer header-field) (length unread)
           unread                      '())
     (go :header-in-line)
   :header-in-line
     (let ((byte (read-byte stream)))
       (case byte
         (#.+cr+
          (assert (= +lf+ (read-byte stream)))
          (push (copy-seq header-field) header-fields)
          (go :header-end-of-line))
         (t
          (vector-push-extend byte header-field)
          (go :header-in-line))))
   :header-end-of-line
     (let ((byte (read-byte stream)))
       (case byte
         (#.+cr+
          (assert (= +lf+ (read-byte stream)))
          (go :body-start))
         (t
          (push byte unread)
          (go :header-start-of-line))))
   :body-start
     (let* ((fields (parse-header-fields header-fields))
            (length (parse-integer (cdr (assoc "Content-Length" fields
                                               :test #'string=))))
            (buffer (make-array length :element-type '(unsigned-byte 8))))
       (read-sequence buffer stream)
       (return (values (sb-ext:octets-to-string buffer) fields)))))

(defun transport/write-response (stream content)
  ;; Write header
  (write-string "Content-Length: " stream)
  (write (length content) :stream stream)
  (write-sequence +crlf*2+ stream)

  ;; Write body
  (write-string content stream)

  ;; Force output
  (force-output stream))
