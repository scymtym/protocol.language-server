;;;; package.lisp --- Package definition for the transport module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.protocol
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:export
   #:parse-position #:unparse-position
   #:parse-range    #:unparse-range
   #:parse-location #:unparse-location)

  (:export
   #:parse-text-document
   #:parse-text-document-content-change)

  ;; Edit
  (:export
   #:range
   #:new-text

   #:edit
   #:make-edit

   #:unparse-edit)

  ;; Message
  (:export
   #:message-action-item
   #:title

   #:message
   #:kind
   #:message
   #:action-items

   #:unparse-message)

  ;; Markup content
  (:export
   #:kind
   #:value

   #:markup-content
   #:make-markup-content

   #:unparse-markup-content)

  ;; Completion item
  (:export
   #:label
   #:kind
   #:filter-text
   #:detail
   #:documentation*
   #:edit
   #:text-format

   #:completion-item
   #:make-completion-item

   #:unparse-completion-item)

  ;; Hover result
  (:export
   #:content
   #:range

   #:hover-result
   #:make-hover-result

   #:unparse-hover-result)

  ;; Highlight
  (:export
   #:kind
   #:range

   #:highlight
   #:make-highlight

   #:unparse-highlight)

  (:export
   #:annotation
   #:message
   #:diagnostic
   ; TODO #:make-diagnostic

   #:unparse-diagnostic
   #:unparse-text-document))
