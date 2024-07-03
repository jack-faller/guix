#!/usr/bin/env guile
!#

;; Script is entered with current tab moved to end.

(define url (getenv "QUTE_URL"))

(set! *random-state* (random-state-from-platform))
;; browser crashes unless you submit all commands at once with ";;"s
(define cmd
  (string-join
   (append
	(apply append
		   (map
			(λ (i)
			  (define j (+ i 1))
			  (define rand (random j))
			  (list
			   (string-append "tab-select " (number->string (+ rand 1)))
			   "tab-move end"))
			(reverse (iota (string->number (getenv "QUTE_TAB_INDEX"))))))
	(list (string-append "tab-select " url)))
   ";; "))

(with-output-to-file (getenv "QUTE_FIFO") (λ () (display cmd)))
