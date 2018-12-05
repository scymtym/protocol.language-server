(cl:in-package #:protocol.language-server.test)

(in-suite :protocol.language-server)

(test root-uri-mixin.construct

  (let+ (((&flet+ check ((initargs expected))
            (let ((workspace (apply #'make-instance 'root-uri-mixin
                                    initargs))
                  (expected  (puri:uri expected)))
              (is (puri:uri= expected (root-uri workspace)))))))
    (mapc #'check '(((:root-path #P"/foo/bar") "file:///foo/bar/")
                    ((:root-uri "/foo/bar")    "/foo/bar/")
                    ((:root-uri #U"/foo/bar")  "/foo/bar/")))))
