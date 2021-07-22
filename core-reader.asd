; vim: ft=lisp et
(in-package :asdf)
(defsystem "core-reader"
  :version "1.0.2"
  :author "SATO Shinichi"
  :source-control (:git "git@github.com:hyotang666/core-reader")
  :bug-tracker "https://github.com/hyotang666/core-reader/issues"
  :description "Utilities for stream oriented reader."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   )
  :components
  ((:file "core-reader")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "core-reader"))))
  (append (call-next-method) '((test-op "core-reader.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "core-reader")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
