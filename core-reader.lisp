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
                                         `(tagbody ,@body))
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
        (eof-value nil) (consume nil) (include nil)
        &aux (pred (coerce pred 'function))) ; canonicalize.
  (loop :for char
             = (handler-case (read-char)
                 (end-of-file (c)
                   (if acc
                       (loop-finish)
                       (if eof-error-p
                           (error c)
                           (return eof-value)))))
        :if (not (funcall pred char))
          :collect char :into acc
        :else :if consume
          :if include
            :collect char :into acc
          :else
            :do (unread-char char)
        :and :do (loop-finish)
        :finally (return (coerce (the list acc) 'string))))

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
    (do-stream-till (c (lambda (c) (char= end-char c)) nil t t)
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

(declaim
 (ftype (function (character &optional boolean) (values function &optional))
        char-pred))

(defun char-pred (char &optional check)
  (assert (typep char 'character))
  (if check
      (lambda (c) (and (characterp c) (char= c char)))
      (lambda (c) (char= c char))))

(define-compiler-macro char-pred (&whole whole char &optional check)
  (if (not (constantp check))
      whole
      (if (not (constantp char))
          (if (typep char '(cons (eql the) (cons character t)))
              `(lambda (c) (char= ,char c))
              (let ((var (gensym "CHAR")))
                `(let ((,var ,char))
                   (assert (typep ,var 'character))
                   ,(if (null check)
                        `(lambda (c) (char= ,var c))
                        `(lambda (c) (and (characterp c) (char= ,var c)))))))
          (etypecase char
            ((or character (cons (eql quote) (cons character null)))
             (if (null check)
                 `(lambda (c) (char= ,char c))
                 `(lambda (c) (and (characterp c) (char= ,char c)))))))))
