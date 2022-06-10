(use-modules (ice-9 textual-ports)
             (guix gexp)
             (gnu services)
             (gnu services web)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu packages vim)
             (gnu packages web)
             (gnu packages ssh)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages admin)
			 (gnu packages version-control))

(define wants-list
  (let ((computer (call-with-input-file "/etc/hostname" get-line)))
	(cond
	 ((equal? computer "mikhail") '(bash server))
	 ((equal? computer "sergey") '(zsh sway))
	 ((equal? computer "birtha") '(zsh bspwm)))))
(define* (wants? x #:optional (else #f))
  (lambda (u) (if (memq x wants-list) u else)))
(define (list-when . args) (filter identity args))

(home-environment
 (packages (list-when
			git
			vim
			glibc
			openssh
			((wants? 'server) nginx)
			(make-glibc-utf8-locales
			 glibc
			 #:locales (list "en_GB" "en_US")
			 #:name "glibc-my-utf8-locales")
			((wants? 'bash) bash)))
 (services
  (list-when
   (service home-shepherd-service-type
			(home-shepherd-configuration))
   ((wants? 'bash)
	(service home-bash-service-type
			 (home-bash-configuration
			  (guix-defaults? #t)
			  (environment-variables '(("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")
									   ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")))
			  (bashrc (list (local-file "bashrc")
							(plain-file "pull-home.sh"
										(string-append "pull-home () { cd '" (canonicalize-path ".") "'; git pull; update-home; }"))))
			  (aliases `(("update-home" . ,(string-append "guix home reconfigure " (canonicalize-path "home.scm")))
						 ("update-guix" . "sudo -i guix pull; guix gc -d 6m -C; systemctl restart guix-daemon.service"))))))
   ;; ((wants? 'server)
   ;; 	(service nginx-service-type
   ;; 			 (nginx-configuration
   ;; 			  (file (local-file "nginx.conf")))))
   )))
