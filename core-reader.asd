; vim: ft=lisp et
(in-package :asdf)
(defsystem "core-reader"
  :author "Shinichi Sato"
  :description "Utilities for stream oriented reader."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   "type-ext" ; type extension.
   )
  :components
  ((:file "core-reader")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "core-reader"))))
  (append (call-next-method)'((test-op "core-reader.test"))))
