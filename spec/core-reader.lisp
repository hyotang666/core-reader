(defpackage :core-reader.spec
  (:use :cl :jingoh #:core-reader))
(in-package :core-reader.spec)
(setup :core-reader)

(requirements-about READ-STRING-TILL :test string=
		    :around (with-input-from-string(*standard-input* "foo bar 666 bazz'")
			      (call-body)))

;;;; Description:

#+syntax
(READ-STRING-TILL pred
		  &optional
		  (*standard-input* *standard-input*)
		  (eof-error-p t)
		  (eof-value nil)
		  (consume nil)
		  (include nil)) ; => result

#?(read-string-till(lambda(c)(char= #\space c)))
=> "foo"
#?(read-string-till #'digit-char-p)
=> "foo bar "
;;;; Arguments and Values:

; pred := function-designator as (function(t)generarized-boolean),
; otherwise error
#?(read-string-till :not-function-designator) :signals error
,:ignore-signals warning

; *standard-input* := input stream, otherwise error.
#?(read-string-till #'digit-char-p *standard-output*) :signals error
#?(read-string-till #'digit-char-p "hogehoge") :signals error
,:lazy T

; eof-error-p := boolean which specifies to signal error or not, when reach end of file.
; The default is T.
#?(with-input-from-string(*standard-input* "")
    (read-string-till (lambda(c)(char= c #\null))))
:signals end-of-file
#?(with-input-from-string(*standard-input* "")
    (read-string-till (lambda(c)(char= c #\null)) *standard-input* nil))
=> NIL
; NOTE! - Even if reach end of file, if have contents, return it successfully.
#?(read-string-till (lambda(c)(char= c #\null))) => "foo bar 666 bazz'"

; eof-value := T, returned when reach end of file.
; The default is nil.
#?(with-input-from-string(s "")
    (read-string-till (lambda(c)(char= c #\null)) s nil :default))
=> :default

; consume := boolean, specify consumes character which satisfies PRED.
; The default is nil.
#?(flet((spacep()
	  (lambda(c)(char= c #\space))))
    (values (read-string-till (spacep))
	    (read-string-till (spacep))))
:values ("foo" "")
,:test equal
#?(flet((spacep()
	  (lambda(c)(char= c #\space))))
    (values (read-string-till (spacep) *standard-input* t t t)
	    (read-string-till (spacep))))
:values ("foo" "bar")
,:test equal

; include := boolean, specify consumed character is included in result.
; The default is T.
#?(flet((spacep()
	  (lambda(c)(char= c #\space))))
    (values (read-string-till (spacep) *standard-input* t t t t)
	    (read-string-till (spacep))))
:values ("foo " "bar")
,:test equal

; result := (or string T) Normally string is expected, but when eof-value specified.

;;;; Affected By:
; `*standard-input*`

;;;; Side-Effects:
; Consume contents of `*standard-input*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DELIMITER)

;;;; Description:
; Return delimiter predicate.

#+syntax
(DELIMITER delimiters) ; => result

#?(delimiter "abcd") :be-the function

;;;; Arguments and Values:

; delimiters := sequence, which includes character.
; when not sequence, signals an error.
#?(delimiter :not-sequence) :signals type-error
,:lazy t
; when element is not character, signals an type-error.
#?(delimiter #(not character elt)) :signals type-error

; result := function as (function(T)boolean)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about READ-DELIMITED-STRING :test string=
		    :around (with-input-from-string(*standard-input* "foo] bar 666 \\'bazz'")
			      (call-body)))

;;;; Description:
; Read delimited strimg.

#+syntax
(READ-DELIMITED-STRING end-char &optional (*standard-input* *standard-input*)
		       start-char) ; => result

#?(read-delimited-string #\space) => " foo] "

;;;; Arguments and Values:

; end-char := character which is delimiter, otherwise error
#?(read-delimited-string :not-character) :signals type-error
,:lazy T

; *standard-input* := input stream, otherwise error
#?(read-delimited-string #\space *standard-output*) :signals error

; start-char := character which is open delimiter, otherwise error.
#?(read-delimited-string #\( *standard-input* :not-character) :signals condition
; If specified, such char use as open delimiter.
#?(read-delimited-string #\]) ; <--- without specify.
=> "]foo]"
#?(read-delimited-string #\] *standard-input* #\[)
=> "[foo]"

; result := string

;;;; Affected By:
; `*standard-input*`

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
; Escaped character is ignored.
#?(read-delimited-string #\')
=> "'foo] bar 666 \\'bazz'"

;;;; Exceptional-Situations:
; When reach end of file, signals an error
#?(read-delimited-string #\`) :signals end-of-file

(requirements-about STRING-CONCAT :test string=)

;;;; Description:
; concatenate strings.
; Less feature, but faster than `UIOP/UTILITY:REDUCE/STRCAT`

#+syntax
(STRING-CONCAT list) ; => result

#?(string-concat '(#\a #\b "cd"))
=> "abcd"
;;;; Arguments and Values:

; list := list which contains character or string.
; If not list, an condition is signaled.
#?(string-concat #(#\a #\b "cd")) :signals error
,:ignore-signals warning
; If element is not (or character string), an error is signaled.
#?(string-concat '(not (or character string))) :signals error

; result := string

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CHAR-PRED)

;;;; Description:
; Return function which test char=.

#+syntax
(CHAR-PRED char &optional check) ; => result

#?(char-pred #\a) :be-the function
#?(funcall (char-pred #\a) #\a) => T
#?(funcall (char-pred #\a) #\b) => NIL
;;;; Arguments and Values:

; char := character which means target.
; if not character, signals an error.
#?(char-pred "a") :signals (or type-error
			       error ; for clisp.
			       )
,:ignore-signals warning

; check := boolean which specify return function should test argument is character or not.
; The default is NIL.
#?(funcall (char-pred #\a) :not-character) :signals type-error
,:lazy t
#?(funcall (char-pred #\a T) :not-character) => NIL

; result := function as (function(character)boolean)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

