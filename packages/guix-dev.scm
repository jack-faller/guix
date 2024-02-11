(define-module (packages guix-dev)
  #:export (guix-dev)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define guix-dev
  (package
   (name "guix-dev")
   (version "0.0.0")
   (source
	(program-file
	 "dev-script"
	 #~(begin
		 (use-modules (ice-9 getopt-long)
					  (srfi srfi-1))
		 (define guix-scm
		   (let loop ((dir (getcwd)))
			 (let ((guixscm (string-append dir "/guix.scm")))
			   (cond
				((file-exists? guixscm) `("-D" "-f" ,guixscm))
				((equal? dir "/") '())
				(else (loop (dirname dir)))))))
		 (define args (program-arguments))
		 (define spec
		   `((rebuild-cache (single-char #\r) (value #f))
			 (print         (single-char #\p) (value #f))
			 (guix          (single-char #\g) (value #t))))
		 (define pos (list-index (lambda (x) (equal? x "--")) args))
		 (define-values (flags command)
		   (if pos (split-at args pos) (values args #f)))
		 (define options (getopt-long flags spec))
		 (define (dev-file f)
		   (string-append #$(load "../get-config-dir.scm") "/dev/" f ".scm"))
		 (define args*
		   `("man-db" "texinfo"
			 ,@(if (option-ref options 'rebuild-cache #f)
				   '("--rebuild-cache") '())
			 ,@(append-map
				(lambda (x)
				  (define file (dev-file x))
				  (if (file-exists? file) (list "-f" file) (list x)))
				(option-ref options '() '()))
			 ,@(let ((opt-guix (option-ref options 'guix #f)))
				 (cond
				  (opt-guix (list "-D" "-f" opt-guix))
				  (guix-scm guix-scm)
				  (else '())))
			 ,@(or command '())))
		 (if (option-ref options 'print #f)
			 (begin (display (string-join args*)) (newline))
			 (apply system* "guix" "shell" args*)))))
   (build-system copy-build-system)
   (arguments '(#:install-plan '(("dev-script" "/bin/dev"))))
   (home-page "https://github.com/jackfaller/guix")
   (synopsis "A script to run guix shell.")
   (description "A script to run guix shell.")
   (license license:cc0)))
guix-dev
