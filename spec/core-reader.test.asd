; vim: ft=lisp et
(in-package :asdf)
(defsystem :core-reader.test
  :depends-on
  (:jingoh "core-reader")
  :components
  ((:file "core-reader"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :core-reader)))