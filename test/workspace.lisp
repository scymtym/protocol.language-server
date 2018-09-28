(cl:in-package #:protocol.language-server.test)

(test root-uri-mixin.construct

      (let+ (((&flet+ check ((initargs expected))
                (is (puri:uri= expected (root-))))))
    (make-instance 'root-uri-mixin :root-path #P"/foo/bar"))
   (make-instance 'root-uri-mixin :root-uri "/foo/bar")
   (make-instance 'root-uri-mixin :root-uri #U"/foo/bar"))
