; vim: ft=lisp et
(in-package :asdf)
(defsystem "core-reader"
  :depends-on
  ("type-ext" "bsearch")
  :components
  ((:file "core-reader")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "core-reader"))))
  (test-system :core-reader.test))