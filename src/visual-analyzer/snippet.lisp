(cl:in-package #:protocol.language-server.visual-analyzer)

(defclass style-table ()
  ())

(service-provider:register-provider/class
 'text.source-location.print::style :table :class 'style-table)

(defmethod text.source-location.print:print-source-using-style ((style style-table) stream source)
  (format stream "In ")
  (let ((name (sloc:name source)))
    (clim:with-drawing-options (stream :text-face :bold)
      (clim:present name (clim:presentation-type-of name) :stream stream)))
  (format stream ":~%"))

(defmethod text.source-location.print:print-line-using-style
    ((style   style-table)
     (stream  t)
     (number  t)
     (content t)
     &key
     (start-column 0)
     end-column
     line-number-width)
  (format stream "~A" content))

(defmethod text.source-location.print:print-line-annotations-using-style
    ((style       style-table)
     (stream      t)
     (number      t)
     (position    t)
     (annotations t)
     &key
     start-column
     end-column
     line-number-width)
  (loop :for previous = (max 0 (1- start-column)) :then (+ end (length (sloc:text annotation)))
        :for (start end annotation) :in annotations
        :when (> previous start)
        :do (setf previous (max 0 (1- start-column)))
        :do (format stream "~V@T" (max 0 (- start previous)))
            (clim:with-drawing-options (stream :ink clim:+deeppink+ :text-face :italic)
              (format stream "~A" (sloc:text annotation)))
            #+no (text.source-location.print:print-line-annotation-using-style
                  style stream (- end start) position annotation)))

(defmethod text.source-location.print:print-annotated-line-using-style
    ((style       style-table)
     (stream      t)
     (number      t)
     (content     t)
     (annotations t)
     &key
     (start-column      0)
     end-column
     line-number-width)
  (clim:formatting-row (stream)
    (clim:formatting-column (stream)
      (clim:formatting-cell (stream)
        (clim:with-drawing-options (stream :ink clim:+darkgray+ :text-size :tiny)
          (format stream "~:D" number))))
    (clim:formatting-column (stream)
      (clim:formatting-cell (stream)
        (clim:with-drawing-options (stream :text-family :fix :text-size :small)
          (let+ (((&flet print-annotations (position)
                    (when-let ((annotations (remove position annotations :test-not #'eq :key #'fourth)))
                      (when (eq position :below)
                        (terpri stream))
                      (text.source-location.print:print-line-annotations-using-style
                       style stream number position annotations
                       :start-column      start-column
                       :end-column        end-column
                       :line-number-width line-number-width)
                      (when (eq position :above)
                        (terpri stream))))))
            (print-annotations :above)
            (text.source-location.print:print-line-using-style
             style stream number content
             :start-column      start-column
             :end-column        end-column
             :line-number-width line-number-width)
            (print-annotations :below)))))))

(defmethod text.source-location.print:print-annotations-using-style
    ((style style-table) stream annotations &key (context-lines 2))
  (let ((clusters (text.source-location.print::cluster-locations
                   annotations
                   :key                     #'sloc:location
                   :intra-cluster-gap-limit (+ 2 (* 2 context-lines)))))
    (map nil (lambda+ ((source . annotations))
               #+no (text.source-location.print:print-source-using-style style stream source)
               (clim:formatting-table (stream)
                 (loop :with end-location = (extremum (map 'list #'sloc:location (lastcar annotations))
                                                      (complement #'sloc:location<)
                                                      :key #'sloc:end)
                       :with text-info = (sloc::text-info (sloc:content (sloc:source end-location)))
                       :with line-number-width
                          = (text.source-location.print::line-number-width
                             (+ (nth-value
                                 4 (text.source-location.print::line-bounds (sloc:range end-location) text-info))
                                context-lines))
                       :for  (annotation next) :on annotations
                       :do   (text.source-location.print::print-annotated-lines-using-style
                              stream style annotation
                              :line-number-width line-number-width
                              :context-lines     context-lines)
                             #+no #+no  :when next
                          #+no #+no :do   (format stream "~@:_~V<⁞~> ⁞  ⁞~@:_~@:_"
                                                  line-number-width)))
               #+no (pprint-newline :mandatory stream))
         clusters)
    (force-output *trace-output*)))

;;;

(clim:define-presentation-method clim:present ((object sloc:annotation) (type document) stream view &key)
  (text.source-location.print:print-annotations-using-style
   (text.source-location.print:make-style :table)
   stream (list object) :context-lines 2))
