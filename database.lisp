; Guide: http://file.allitebooks.com/20160121/Practical%20Common%20LISP.pdf

; A small introduction to LISP
; with a simple database to store CD/MP3 entries

; create a property list (plist)
; which holds the information of our CD
; / an entitiy in our database
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; define a global variable
; global variables have by convention asterisk at start and end of name
(defvar *db* nil)

; define a function with "cd" as parameter
(defun add-record (cd) (push cd *db*))


(defun dump-db ()
  ; dolist = iterate over list
  ; "cd" is the object with the current list iteration item
  (dolist (cd *db*)
	(format t "~{~a:~10t~a~%~}~%" cd)))

; format string explained:
; ~ is always a special action
; ~{ = start loop over a list
; ~} = end loop over a list
; ~a = Human readable form of a string (without quotes) or name (without :)
; ~10t = tab of 10
; ~% = new line
(defun dump-db-crazy ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

; function to prompt on terminal and read the user input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  ; force-output is necessary on some LISP implementations to ensure LISP doesnt wait for a newline
  ; before printing the prompt
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
	(prompt-read "Title")
	(prompt-read "Artist")
	; if it can not parse the input to an int it will return NIL
	; without ":junk-allowed" it would signal an error 
	; ":junk-allowed" is an optional keyword
	(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) ; or = will take the prompt input OR 0 if prompt is NIL (not parsable)
	; y-or-n-p is built-in ... how handy
	(y-or-n-p "Ripped: ")))

(defun add-cds ()
  ; loop till return
  (loop (add-record (prompt-for-cd))
		; if n => return out of loop
		(if (not (y-or-n-p "Another?: ")) (return))))

; save db to disk
(defun save-db (filename)
  ; open a file and bind stream to "out" variable
  ; filename is the path to the file
  ; :direction :output => write to file
  ; :if-exists :supersede => override existing file
  (with-open-file (out filename :direction :output :if-exists :supersede)
	; ensure that certain variables that affect the behavior of print are set to their standard values
	(with-standard-io-syntax
	  ; output *db* in a form which LISP can also read back
	  (print *db* out))))

; load db (file) back to lisp
(defun load-db (filename)
  ; default :direction is :input
  (with-open-file (in filename)
	; read with same basic syntax our save-db function was using
	(with-standard-io-syntax
	  ; read => same reader as used by REPL
	  ; setf => assignment operator; set first arg to the result of evaluating the second arg
	  (setf *db* (read in)))))


; filter list (search for artist)
;(defun select-by-artist (artist)
  ;; filter list (create new list)
  ;(remove-if-not
	;; anon function (lambda) are specified by name "lambda" - syntax is equal to defun
	;; with "#" you specify to refer to a function and not the value of a variable
	;#'(lambda (cd) (equal artist (getf cd :artist)))' *db* ))

; pass a function to select
; a more general function to replace "select-by-artist"
(defun select (selector-fn)
  ; filter with selector-fn
  (remove-if-not selector-fn *db*))

; return lambda function to select the artist
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

; an even more general approach for selectors
; using keyword parameters
;
; (ripped nil ripped-p)
; -> nil is the default
; ripped-p is a flag which indicates if the NIL is from the USER or default
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
		(if title (equal (getf cd :title) title) t)
		(if artist (equal (getf cd :artist) artist) t)
		(if rating (equal (getf cd :rating) rating) t)
		(if ripped-p (equal (getf cd :ripped) ripped) t))))

; example usage
; (select (where :artist "Ich"))
; or
; (select (where :rating 11))
; or
; (select (where :artist "Ich" :ripped nil))


; update entries in the database
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
		(mapcar
		  #'(lambda (row)
			  (when (funcall selector-fn row)
				(if title (setf (getf row :title) title))
				(if artist (setf (getf row :artist) artist))
				(if rating (setf (getf row :rating) rating))
				(if ripped-p (setf (getf row :ripped) ripped)))
			  row) *db*)))

; example usage
; (update (where :artist "Ich") :rating 11)
