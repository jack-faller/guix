(use-modules (ice-9 textual-ports))

(define wants-list
  (let ((computer (call-with-input-file "/etc/hostname" get-line)))
	(cond
	 ((equal? computer "mikhail") '(bash))
	 ((equal? computer "sergey") '(zsh))
	 ((equal? computer "birtha") '(zsh)))))
(define (wants? x) (memq x wants-list))
