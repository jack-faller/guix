(define wants-list
  (let ((computer (call-with-input-file (canonicalize-path "hostname") get-line)))
	(cond
	 ((equal? computer "mikhail") '(bash server))
	 ((equal? computer "sergey") '(bash zsh sway))
	 ((equal? computer "birtha") '(zsh bspwm)))))
(define* (wants? x #:optional (else #f))
  (lambda (u) (if (memq x wants-list) u else)))
(define (list-when . args) (filter identity args))
