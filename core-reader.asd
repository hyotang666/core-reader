; vim: ft=lisp et
(in-package :asdf)
(defsystem "core-reader"
  :version "0.0.1"
  :author "Shinichi Sato"
  :description "Utilities for stream oriented reader."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   )
  :components
  ((:file "core-reader")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "core-reader"))))
  (append (call-next-method)'((test-op "core-reader.test"))))
(defmethod operate :around (o (c (eql (find-system "core-reader")))
                              &key ((:compile-print *compile-print*))
                              ((:compile-verbose *compile-verbose*))
                              &allow-other-keys)
  (call-next-method))
