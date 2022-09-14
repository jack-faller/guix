(define wants-list
  (let ((computer (call-with-input-file (canonicalize-path "hostname") get-line)))
	(cond
	 ((equal? computer "mikhail") '(bash server))
	 ((equal? computer "sergey") '(zsh sway))
	 ((equal? computer "birtha") '(zsh bspwm))
	 ((equal? computer "test") '(bash bspwm)))))
(define* (wants? x #:optional (else #f))
  (lambda (u) (if (memq x wants-list) u else)))
(define (list-when . args) (filter identity args))
