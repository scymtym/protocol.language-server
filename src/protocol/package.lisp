;;;; package.lisp --- Package definition for the protocol module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocol.language-server.protocol
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:shadow
   #:documentation)

  ;; Serialization protocol
  (:export
   #:parse
   #:unparse)

  ;; TODO which of these are deprecated?
  (:export
   #:parse-position #:unparse-position
   #:parse-range    #:unparse-range
   #:parse-location #:unparse-location)

  (:export
   #:parse-text-document
   #:parse-text-document-content-change)

  ;; Location link
  (:export
   #:origin-selection-range
   #:target-uri
   #:target-range
   #:target-selection-range

   #:location-link
   #:make-location-link)

  ;; Edit
  (:export
   #:range
   #:new-text

   #:edit
   #:make-edit

   #:unparse-edit)

  ;; Text document edit
  (:export
   #:document
   #:edits

   #:make-text-document-edit)

  (:export
   #:document-changes

   #:workspace-edit
   #:make-workspace-edit)

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

  ;; Document symbol
  (:export
   #:name
   #:detail
   #:kind
   #:deprecated?
   #:range
   #:selection-range
   #:children

   #:make-document-symbol)

  ;; Symbol information
  (:export
   #:name
   #:kind
   #:deprecated?
   #:location
   #:container-name

   #:make-symbol-information)

  ;; Diagnostic
  (:export
   #:annotation

   #:message
   #:make-message

   #:diagnostic
                                        ; TODO #:make-diagnostic

   #:unparse-diagnostic
   #:unparse-text-document) ; TODO why is this here?

  ;; Command
  (:export
   #:title
   #:command
   #:arguments

   #:command
   #:make-command)

  ;; Code action
  (:export
   #:title
   #:command
   #:arguments

   #:code-action
   #:make-code-action

   #:unparse-code-action))
