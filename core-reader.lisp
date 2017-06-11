(defpackage #:core-reader
  (:use #:common-lisp #:bsearch)
  (:import-from :type-ext #:prototype)
  (:export
    #:read-string-till
    #:delimiter
    #:read-delimited-string
    #:string-concat
    #:char-pred
    ))
(in-package #:core-reader)

(deftype function-designator()
  '(or function
       (and symbol (not (or boolean keyword)))))

(deftype input-stream()
  '(and stream (satisfies input-stream-p)))

(Prototype read-string-till(function-designator
			    &optional input-stream boolean T boolean boolean)
	   (values (or string t) boolean))
(defun read-string-till (pred &optional
			      (*standard-input* *standard-input*)
			      (eof-error-p t)
			      (eof-value nil)
			      (consume nil)
			      (include nil))
  (loop :for (c condition) = (multiple-value-list(ignore-errors(read-char)))
	;; C is character or nil.
	:while(or (and c (null(funcall pred c)))
		  (and (null c) ; reach end of file.
		       (if result
			 (return(coerce result 'string))
			 (if eof-error-p
			   (error condition)
			   (return eof-value)))))
	:collect c :into result ; always consume one char.
	:when(char= c #\\) ; escape char-p
	:collect (read-char) :into result ; consume one more char.
	:finally(return (if consume
			  (if include
			    (concatenate 'string result (string c))
			    (coerce result 'string))
			  (progn(unread-char c)
			    (coerce result 'string))))))

(Prototype delimiter(vector)function)
(defun delimiter(delimiters)
  (lambda(c)
    (Bsearch c delimiters :compare #'char< :test #'char=)))

(Prototype read-delimited-string(character &optional input-stream)
	   string)
(defun read-delimited-string(end-char &optional (*standard-input* *standard-input*))
  (loop :for c = (read-char)
	:collect c :into result
	:if (char= #\\ c) :collect (read-char) :into result
	:else :if (char= end-char c)
	:return (concatenate 'string(string c) result)))

(Prototype string-concat(list)string)
(defun string-concat(list)
  (let*((size(let((size 0))
	       (declare(type fixnum size))
	       (dolist(elt list size)
		 (if(characterp elt)
		   (incf size)
		   (incf size (length elt))))))
	(string(make-array size :element-type 'character)))
    (loop :for elt :in list
	  :with index = 0
	  :when (characterp elt)
	  :do (setf(schar string index)elt)
	  (incf index)
	  :else :do (replace string elt :start1 index)
	  (incf index (length elt)))
    string))

(Prototype char-pred(character &optional boolean)function)
(defun char-pred(char &optional check)
  (assert(typep char 'character))
  (if check
    (lambda(c)
      (and (characterp c)
	   (char= c char)))
    (lambda(c)
      (char= c char))))
(define-compiler-macro char-pred(&whole whole char &optional check)
  (if(not(constantp check))
    whole
    (if(not(constantp char))
      (if(typep char '(CONS (EQL THE) (CONS CHARACTER T)))
	`(LAMBDA(C)(CHAR= ,char C))
	(let((var(gensym"CHAR")))
	  `(LET((,var ,char))
	     (ASSERT(TYPEP ,var 'CHARACTER))
	     ,(if(null check)
		`(LAMBDA(C)(CHAR= ,var C))
		`(LAMBDA(C)
		   (AND (CHARACTERP C)
			(CHAR= ,var C)))))))
      (etypecase char
	((or CHARACTER
	     (CONS (EQL QUOTE) (CONS CHARACTER NULL)))
	 (if(null check)
	   `(LAMBDA(C)(CHAR= ,char C))
	   `(LAMBDA(C)
	      (AND (CHARACTERP C)
		   (CHAR= ,char C)))))))))
