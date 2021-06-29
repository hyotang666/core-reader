(defpackage #:core-reader
  (:use :cl)
  (:export #:read-string-till
           #:delimiter
           #:read-delimited-string
           #:string-concat
           #:char-pred))

(in-package #:core-reader)

(deftype function-designator ()
  '(or function (and symbol (not (or boolean keyword)))))

(declaim
 (ftype (function
         (function-designator &optional stream boolean t boolean boolean)
         (values (or string t) boolean))
        read-string-till))

(declaim (inline %read-string-till))

(defun %read-string-till
       (pred *standard-input* eof-error-p eof-value consume include)
  (declare (optimize speed)
           (type function pred))
  (prog (result)
    (declare (type list result))
    (handler-bind ((end-of-file
                    (lambda (c)
                      (declare (ignore c))
                      (if result
                          (go :return)
                          (unless eof-error-p
                            (return eof-value))))))
      (tagbody
       :top
        (push (read-char) result)
        (unless (funcall pred (car result))
          (go :top))
        (if consume
            (unless include
              (pop result))
            (unread-char (pop result)))))
   :return
    (return (coerce (nreverse result) 'string))))

(defun read-string-till
       (pred
        &optional (*standard-input* *standard-input*) (eof-error-p t)
        (eof-value nil) (consume nil) (include nil))
  (%read-string-till (coerce pred 'function) *standard-input* eof-error-p
                     eof-value consume include))

(define-compiler-macro read-string-till
    (function &optional stream (eof-error-p t) eof-value consume include)
  `(%read-string-till
     ,(typecase function
        ((cons (eql quote) (cons symbol null)) `#',(cadr function))
        ((cons (eql function) t) function)
        ((cons (eql the) (cons function t)) function)
        (t `(coerce ,function 'function)))
     ,(or stream '*standard-input*) ,eof-error-p ,eof-value ,consume ,include))

(declaim (ftype (function (sequence) (values function &optional)) delimiter))

(defun delimiter (delimiters)
  (let ((ht (make-hash-table)))
    (map nil
         (if (stringp delimiters)
             (lambda (char) (setf (gethash char ht) char))
             (lambda (char)
               (check-type char character)
               (setf (gethash char ht) char)))
         delimiters)
    (lambda (char) (values (gethash char ht)))))

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
  (loop :for c :of-type character = (read-char)
        :collect c :into result
        :if (char= #\\ c)
          :collect (read-char) :into result
        :else :if (char= end-char c)
          :return (concatenate 'string (string (or start-char c)) result)))

(declaim (ftype (function (list) (values string &optional)) string-concat))

(defun string-concat (list)
  (declare (optimize (speed 3)))
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

(define-compiler-macro char-pred
    (&whole whole char &optional check)
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