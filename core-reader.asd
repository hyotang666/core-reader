; vim: ft=lisp et
(in-package :asdf)
(defsystem "core-reader"
  :author "Shinichi Sato"
  :description "Core helpers for writing reader."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  ("type-ext" "bsearch")
  :components
  ((:file "core-reader")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "core-reader"))))
  (test-system :core-reader.test))
