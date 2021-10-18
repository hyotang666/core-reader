(defpackage #:core-reader
  (:use :cl)
  (:export #:read-string-till
           #:delimiter
           #:do-stream
           #:do-stream-till
           #:do-stream-till-suffix
           #:read-delimited-string
           #:string-concat
           #:char-pred))

(in-package #:core-reader)

(declaim (optimize speed))

(deftype function-designator ()
  '(or function (and symbol (not (or boolean keyword)))))

(defmacro do-stream
          ((var &optional stream (eof-error-p t) eof-value) &body body)
  ;; Trivial syntax check.
  (check-type var symbol)
  (multiple-value-bind (forms decls)
      (uiop:parse-body body)
    (let ((s (gensym "STREAM")) (c (gensym "CONDITION")))
      `(let ((,s ,stream))
         (loop (handler-case (read-char ,s)
                 ,(if (constantp eof-error-p)
                      (let ((eof-error-p (eval eof-error-p)))
                        (if eof-error-p
                            `(end-of-file (,c) (error ,c))
                            `(end-of-file nil (return ,eof-value))))
                      `(end-of-file (,c)
                        (if ,eof-error-p
                            (error ,c)
                            (return ,eof-value))))
                 (:no-error (,var)
                   ,@decls
                   (tagbody ,@forms))))))))

(defmacro do-stream-till
          ((var pred &optional stream consume include) &body body)
  ;; Trivial syntax check.
  (check-type var (and symbol (not boolean)))
  (let ((vpred (gensym "PRED")) (s (gensym "INPUT")))
    (multiple-value-bind (body decls)
        (uiop:parse-body body)
      (when decls
        (error "Declaration is not supported."))
      `(let ((,s ,stream) (,vpred (coerce ,pred 'function)))
         (do-stream (,var ,s)
           (cond
             ((funcall ,vpred ,var)
              ,(if (constantp consume)
                   (if (constantp include)
                       (let ((consume (eval consume)) (include (eval include)))
                         (if consume
                             (if include
                                 `(tagbody ,@body)
                                 `(return nil))
                             `(unread-char ,var ,s)))
                       (let ((consume (eval consume)))
                         (if consume
                             `(if ,include
                                  (tagbody ,@body))
                             `(unread-char ,var ,s))))
                   (if (constantp include)
                       (let ((include (eval include)))
                         `(if consume
                              ,(if include
                                   `(tagbody ,@body))
                              (unread-char ,var ,s)))
                       `(if consume
                            (if include
                                (tagbody ,@body))
                            (unread-char ,var ,s))))
              (return))
             (t (tagbody ,@body))))))))

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
      (lambda (c) #+allegro (check-type c character) (char= c char))))

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
  #+(or ccl abcl)
  (check-type end-char character)
  (let* ((acc (cons nil nil)) (acc-tail acc))
    (do-stream-till (c (char-pred end-char) nil t t)
      (rplacd acc-tail (setf acc-tail (list c)))
      (when (char= #\\ c)
        (rplacd acc-tail (setf acc-tail (list (read-char))))))
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

(defmacro do-stream-till-suffix
          ((var suffix &key ((:stream input)) (include t)) &body body)
  ;; Trivial syntax check.
  (check-type var symbol)
  (multiple-value-bind (forms decls)
      (uiop:parse-body body)
    `(block nil
       (call-with-do-stream-till-suffix ,suffix ,input
                                        (lambda (,var)
                                          ,@decls
                                          (tagbody ,@forms))
                                        :include ,include))))

(defun call-with-do-stream-till-suffix
       (suffix input thunk
        &key (include t)
        &aux (thunk (coerce thunk 'function))
        (input
         (etypecase input
           (stream input)
           ((eql t) *terminal-io*)
           (null *standard-input*))))
  (declare (optimize (speed 1))) ; due to accept sequence.
  (assert (<= 2 (length suffix)))
  (let* ((head (cons :head nil)) (tail head) (first-char (elt suffix 0)))
    (declare (character first-char))
    #+(or ccl allegro)
    (check-type first-char character)
    (labels ((first-char-p (c)
               (char= c first-char))
             (skip ()
               (do-stream-till (c #'first-char-p input)
                 (funcall thunk c)))
             (reset ()
               (loop :for cons :on (cdr head)
                     :do (rplaca cons (read-char input))))
             (shift ()
               (funcall thunk (cadr head))
               (setf (cadr head) (read-char input)
                     (cdr tail) (cdr head)
                     (cdr head) (cddr head)
                     tail (rplacd (cdr tail) nil))))
      (skip)
      ;; Initialize.
      (dotimes (x (length suffix))
        (rplacd tail (setf tail (list (read-char input)))))
      (loop (if (every #'char= suffix (cdr head))
                (progn
                 (when include
                   (mapc thunk (cdr head)))
                 (return))
                (let ((pos (position first-char head :start 2)))
                  (if pos
                      (dotimes (x (1- pos)) (shift))
                      (progn (mapc thunk (cdr head)) (skip) (reset)))))))))