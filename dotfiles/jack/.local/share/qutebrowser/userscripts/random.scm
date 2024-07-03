#!/usr/bin/env guile
!#

;; Script is entered with the last tab open.

(set! *random-state* (random-state-from-platform))
(define tabs (string->number (getenv "QUTE_TAB_INDEX")))
(define cmd
  (string-append "tab-focus " (number->string (+ (random tabs) 1)) "\n"))

(with-output-to-file (getenv "QUTE_FIFO") (λ () (display cmd)))
