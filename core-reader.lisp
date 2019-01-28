(defpackage #:core-reader
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

(Prototype read-string-till(function-designator
			    &optional stream boolean T boolean boolean)
	   (values (or string t) boolean))
(defun read-string-till(pred &optional
			     (*standard-input* *standard-input*)
			     (eof-error-p t)
			     (eof-value nil)
			     (consume nil)
			     (include nil))
  (%read-string-till (coerce pred 'function)
		     *standard-input*
		     eof-error-p
		     eof-value
		     consume
		     include))

(define-compiler-macro read-string-till(function &optional
						 stream
						 (eof-error-p t)
						 eof-value
						 consume
						 include)
  `(%read-string-till ,(typecase function
			 ((cons (eql quote)(cons symbol null))
			  `#',(cadr function))
			 ((cons (eql function) t)
			  function)
			 (t `(coerce ,function 'function)))
		      ,(or stream '*standard-input*)
		      ,eof-error-p
		      ,eof-value
		      ,consume
		      ,include))

(declaim(inline %read-string-till))
(defun %read-string-till (pred *standard-input*
			       eof-error-p
			       eof-value
			       consume
			       include)
  (declare (optimize speed)
	   (type function pred))
  (prog(result)
    (declare(type list result))
    (handler-bind((end-of-file (lambda(c)
				 (declare(ignore c))
				 (if result
				   (go :return)
				   (unless eof-error-p
				     (return eof-value))))))
      (tagbody
	:TOP
	(push (read-char) result)
	(when(char= #\\ (car result))
	  (push (read-char)result)
	  (go :top))
	(unless(funcall pred (car result))
	  (go :top))
	(if consume
	  (unless include
	    (pop result))
	  (unread-char (pop result)))))
    :return
    (return(coerce (nreverse result)'string))))

(Prototype delimiter(sequence)function)
(defun delimiter(delimiters)
  (let((ht(make-hash-table)))
    (map nil (if(stringp delimiters)
	       (lambda(char)
		 (setf(gethash char ht)char))
	       (lambda (char)
		 (check-type char character)
		 (setf (gethash char ht)char)))
	 delimiters)
    (lambda(char)
      (gethash char ht))))

(Prototype read-delimited-string(character &optional stream)
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
