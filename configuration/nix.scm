(define-module (configuration nix)
  #:export (nix-packages-service-type nix-system-services)

  #:use-module (file-utils)

  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services nix))

(define nix-packages-service-type
  (service-type
   (name 'nix-packages)
   (compose concatenate)
   (extend append)
   (description
	"Like home-files-service-type, but when a string is provided, take it recursively from the files directory.")
   (extensions
	(list
	 (service-extension
	  home-shell-profile-service-type
	  (const (list (plain-file
					"nix-source-command"
					"source /run/current-system/profile/etc/profile.d/nix.sh"))))
	 (service-extension
	  home-activation-service-type
	  (λ (packages)
		(with-imported-modules
		 '((ice-9 popen) (ice-9 rdelim) (ice-9 textual-ports))
		 #~(begin
			 (use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 textual-ports))
			 (define packages '#$(sort packages string<))
			 (define package-file
			   (string-append (getenv "HOME") "/.cache/installed-nix-packages"))
			 (define (invoke . args)
			   (unless (= 0 (apply system* args))
				 (error "failed to run nix command" args)))
			 (invoke #$(executable-shell-script
						"nix-add-channels"
						"if [ \"$(nix-channel --list)\" = \"\" ]; then"
						"  nix-channel --add https://nixos.org/channels/nixpkgs-unstable || exit 1"
						"  nix-channel --update || exit 1"
						"  nix-env -iA nixpkgs.nix nixpkgs.cacert || exit 1"
						"  echo '()' > ~/.cache/installed-nix-packages"
						"fi"))
			 (setenv "NIXPKGS_ALLOW_UNFREE" "1")
			 (unless (equal? packages (with-input-from-file package-file read))
			   (display "installing nix packages") (newline)
			   (if (null? packages)
				   (invoke "nix-env" "--uninstall" ".*")
				   (apply invoke "nix-env" "--remove-all" "--install"
						  packages))
			   (with-output-to-file package-file
				 (lambda () (write packages) (newline))))))))))))

(define nix-system-services
  (list
   (service nix-service-type)
   (simple-service 'nix-system-packages profile-service-type (packages "nix"))))
