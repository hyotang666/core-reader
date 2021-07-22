(defpackage #:core-reader
  (:use :cl)
  (:export #:read-string-till
           #:delimiter
           #:do-stream-till
           #:read-delimited-string
           #:string-concat
           #:char-pred))

(in-package #:core-reader)

(declaim (optimize speed))

(deftype function-designator ()
  '(or function (and symbol (not (or boolean keyword)))))

(defmacro do-stream-till
          ((var pred &optional stream consume include) &body body)
  (let ((vpred (gensym "PRED")))
    `(let (,@(when stream
               `((*standard-input* ,stream)))
           (,vpred (coerce ,pred 'function)))
       (loop :for ,var :of-type character := (read-char)
             :until (funcall ,vpred ,var)
             :if (char= #\\ ,var)
               :do (tagbody ,@body)
                   (setf ,var (read-char))
                   (tagbody ,@body)
             :else
               :do (tagbody ,@body)
             :finally ,(if (constantp consume)
                           (if (constantp include)
                               (let ((consume (eval consume))
                                     (include (eval include)))
                                 (if consume
                                     (if include
                                         `(tagbody ,@body)
                                         `(return nil))
                                     `(unread-char ,var)))
                               (let ((consume (eval consume)))
                                 (if consume
                                     `(if ,include
                                          (tagbody ,@body))
                                     `(unread-char ,var))))
                           (if (constantp include)
                               (let ((include (eval include)))
                                 `(if consume
                                      ,(if include
                                           `(tagbody ,@body))
                                      (unread-char ,var)))
                               `(if consume
                                    (if include
                                        (tagbody ,@body))
                                    (unread-char ,var))))))))

(declaim
 (ftype (function
         (function-designator &optional stream boolean t boolean boolean)
         (values (or string t) boolean))
        read-string-till))

(defun read-string-till
       (pred
        &optional (*standard-input* *standard-input*) (eof-error-p t)
        (eof-value nil) (consume nil) (include nil))
  (prog* ((acc (cons nil nil)) (tail acc))
    (handler-case
        (do-stream-till (c pred nil consume include)
          (rplacd tail (setf tail (list c))))
      (end-of-file (c)
        (if (cdr acc)
            (go :return)
            (if eof-error-p
                (error c)
                (return eof-value)))))
   :return
    (return (coerce (the list (cdr acc)) 'string))))

(declaim (ftype (function (sequence) (values function &optional)) delimiter))

(defun delimiter (delimiters)
  (let ((ht (make-hash-table)))
    (map nil
         (if (stringp delimiters)
             (lambda (char) (setf (gethash char ht) t))
             (lambda (char)
               (check-type char character)
               (setf (gethash char ht) t)))
         delimiters)
    (flet ((delimiterp (char)
             (values (gethash char ht))))
      (declare
        (ftype (function (character) (values boolean &optional)) delimiterp))
      #'delimiterp)))

(declaim
 (ftype (function (character &optional boolean) (values function &optional))
        char-pred))

(defun char-pred (char &optional check)
  (assert (typep char 'character))
  (if check
      (lambda (c) (and (characterp c) (char= c char)))
      (lambda (c) (char= c char))))

(define-compiler-macro char-pred (&whole whole char &optional check)
  (flet ((<fun-with-check> (char)
           (let ((var (gensym "VAR")))
             `(flet ((char-pred (,var)
                       (and (characterp ,var) (char= ,char ,var))))
                (declare
                  (ftype (function (t) (values boolean &optional)) char-pred))
                #'char-pred)))
         (<fun-without-check> (char)
           (let ((var (gensym "VAR")))
             `(flet ((char-pred (,var)
                       (char= ,char ,var)))
                (declare
                  (ftype (function (character) (values boolean &optional))
                         char-pred))
                #'char-pred)))
         (<may-assert> (form var)
           (unless (typep form
                          '(cons (member the #+sbcl sb-ext:truly-the)
                                 (cons (member character base-char))))
             `((check-type ,var character)))))
    (if (constantp char)
        (if (constantp check)
            (let ((char (eval char)) (check (eval check)))
              (check-type char character)
              (if check
                  (<fun-with-check> char)
                  (<fun-without-check> char)))
            (let ((char (eval char)))
              (check-type char character)
              `(if ,check
                   ,(<fun-with-check> char)
                   ,(<fun-without-check> char))))
        (if (constantp check)
            (let ((check (eval check)) (var (gensym "CHAR")))
              (if check
                  `(let ((,var ,char))
                     ,@(<may-assert> char var)
                     ,(<fun-with-check> var))
                  `(let ((,var ,char))
                     ,@(<may-assert> char var)
                     ,(<fun-without-check> var))))
            whole))))

(declaim
 (ftype (function (character &optional stream (or null character))
         (values string &optional))
        read-delimited-string))

(defun read-delimited-string
       (end-char &optional (*standard-input* *standard-input*) start-char)
  (declare (type character end-char)
           (type (or null character) start-char))
  #+ccl
  (check-type end-char character)
  (let* ((acc (cons nil nil)) (acc-tail acc))
    (do-stream-till (c (char-pred end-char) nil t t)
      (rplacd acc-tail (setf acc-tail (list c))))
    (rplaca acc (or start-char end-char))
    (coerce acc 'string)))

(declaim (ftype (function (list) (values string &optional)) string-concat))

(defun string-concat (list)
  (let* ((size
          (let ((size 0))
            (declare (type (mod #.most-positive-fixnum) size))
            (dolist (elt list size)
              (etypecase elt
                (character (incf size))
                (string (incf size (length elt)))))))
         (string (make-array size :element-type 'character))
         (index 0))
    (declare (type (mod #.most-positive-fixnum) size index))
    (dolist (elt list string)
      (etypecase elt
        (character (setf (char string index) elt) (incf index))
        ((simple-array character (*))
         (replace string elt :start1 index)
         (incf index (length elt)))))))