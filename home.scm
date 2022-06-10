(use-modules (ice-9 textual-ports)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages base)
             (gnu packages git)
             (gnu packages vim)
			 (guix gexp))

(define my-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "en_GB" "en_US")
   #:name "glibc-canadian-utf8-locales"))

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
			my-glibc-locales
			((wants? 'bash) bash)))
 (services
  (list-when
   ((wants? 'bash)
	(service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (environment-variables '(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")
									  ("GUIX_LOCPATH" . "$HOME/.guix-profile/lib/locale")))
			 (bashrc (local-file ".bashrc"))
			 (aliases '(("update-guix" . "sudo -i guix pull")))))))))
