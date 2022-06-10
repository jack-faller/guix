(use-modules (ice-9 textual-ports)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages git)
             (gnu packages vim)
			 (guix gexp))

(define wants-list
  (let ((computer (call-with-input-file "/etc/hostname" get-line)))
	(cond
	 ((equal? computer "mikhail") '(bash))
	 ((equal? computer "sergey") '(zsh sway))
	 ((equal? computer "birtha") '(zsh bspwm)))))
(define (wants? x)
  (lambda (u) (if (memq x wants-list) u #f)))
(define (list-when . args) (filter identity args))

(home-environment
 (packages (list-when
			git
			vim
			((wants? 'bash) bash)))
 (services
  (list-when
   ((wants? 'bash)
	(service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile '("export HISTFILE=$XDG_CACHE_HOME/.bash_history"))
			 (bashrc (local-file ".bashrc"))))))))
