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
		 (define guix-scm
		   (let loop ((dir (getcwd)))
			 (let ((guixscm (string-append dir "/guix.scm")))
			   (cond
				((file-exists? guixscm) `("-D" "-f" ,guixscm))
				((equal? dir "/") '())
				(else (loop (dirname dir)))))))
		 (define args
		   (let loop ((args (cdr (program-arguments)))
					  (acc '())
					  (guix-scm-used? #f))
			 (if (null? args)
				 (append (apply append (reverse acc))
						 (if guix-scm-used? '() guix-scm))
				 (let* ((arg (car args))
						(file (string-append #$(load "../get-config-dir.scm") "/dev/" arg ".scm")))
				   (cond
					((or (equal? arg "--rebuild-cache") (equal? arg "-r"))
					 (loop (cdr args) (cons '("--rebuild-cache") acc)
						   guix-scm-used?))
					((equal? arg "--")
					 (append (apply append (reverse acc))
							 (if guix-scm-used? '() guix-scm)
							 args))
					((file-exists? file)
					 (loop (cdr args) (cons (list "-f" file) acc)
						   guix-scm-used?))
					((or (equal? arg "--guix") (equal? arg "-g"))
					 (if (and guix-scm (not guix-scm-used?))
						 (loop (cdr args) (cons guix-scm acc) #t)
						 (error (if guix-scm
									"Duplicate --guix"
									"Could not find guix.scm"))))
					(else
					 (loop (cdr args) (cons (list arg) acc) guix-scm-used?)))))))
		 (apply system* "guix" "shell" args))))
   (build-system copy-build-system)
   (arguments '(#:install-plan '(("dev-script" "/bin/dev"))))
   (home-page "https://github.com/jackfaller/guix")
   (synopsis "A script to run guix shell.")
   (description "A script to run guix shell.")
   (license license:cc0)))
guix-dev
