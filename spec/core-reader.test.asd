; vim: ft=lisp et
(in-package :asdf)
(defsystem :core-reader.test
  :version "0.1.0"
  :depends-on
  (:jingoh "core-reader")
  :components
  ((:file "core-reader"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :core-reader)))
