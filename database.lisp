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
	(prompt-read "Rating")
	(prompt-read "Ripped [y/n]")))

