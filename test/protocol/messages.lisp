(cl:in-package #:protocol.language-server.protocol.test)

(def-suite* :protocol.language.server.protocol.messages
  :in :protocol.language-server.protocol)

;;; Position type

(test parse-position.smoke

  #+later (parse (json:decode-json-from-string "{\"foo\":1}") 'text.source-location:))

;; Range type

(test parse-range.smoke

  (cl-json:encode-json-to-string
   (unparse
    (parse (json:decode-json-from-string "{\"start\":{\"line\":1,\"character\":2},
                                           \"end\":  {\"line\":3,\"character\":4}}")
           'text.source-location:range))))

;;; Location type

(test parse-location.smoke

  (cl-json:encode-json-to-string
   (unparse
    (parse
     (json:decode-json-from-string "{\"uri\": \"foo.bar\",
                                   \"range\": {\"start\":{\"line\":1,\"character\":2},
                                               \"end\":  {\"line\":3,\"character\":4}}}")
     'text.source-location:location))))
