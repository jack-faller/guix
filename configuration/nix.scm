(define-module (configuration nix)
  #:export (generate-nix-packages-service nix-packages-service-type nix-system-services)

  #:use-module (file-utils)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services nix))

(define nix-profile-service-type
  (let* ((switch-to-generstion
		  (λ (generation)
			#~(system* "nix-env" "--switch-generation" #$generation)))
		 (nix-profile
		  (plain-file
		   "nix-source-command"
		   "source /run/current-system/profile/etc/profile.d/nix.sh"))
		 (profile-extension
		  (const (list nix-profile))))
	(service-type
	 (name 'nix-profile)
	 (extensions
	  (list
	   (service-extension home-activation-service-type switch-to-generstion)
	   (service-extension home-shell-profile-service-type profile-extension)))
	 (description
	  "A home service that switches to a generation of the user's nix profile.
Its value is a string containing the number of the generation to switch to."))))
;; run a script to install the packages
(define (generate-nix-packages-service . packages)
  (define (assert-nix-success return-code)
	(unless (= 0 return-code)
	   (error "failed to run nix command")))
  (define package-file
	(string-append (getenv "HOME") "/.cache/installed-nix-packages"))
  (assert-nix-success
   (system
	(lines
	 "if [ \"$(nix-channel --list)\" = \"\" ]; then"
	 "  nix-channel --add https://nixos.org/channels/nixpkgs-unstable"
	 "  nix-channel --update"
	 "  nix-env -iA nixpkgs.nix nixpkgs.cacert"
	 (string-append "echo '()' > " package-file)
	 "fi")))
  (unless (equal? packages (with-input-from-file package-file read))
	(with-output-to-file package-file (λ () (write packages) (newline)))
	(let* ((cmd (cons*
				 "NIXPKGS_ALLOW_UNFREE=1" "nix-env" "--remove-all" "--install"
				 packages))
		   (cmd (if (null? packages) '("nix-env" "--uninstall" "'.*'") cmd))
		   (cmd (string-join cmd " ")))
	  (assert-nix-success (system cmd))))
  (service
   nix-profile-service-type
   (let* ((pipe
		   (open-input-pipe
			"nix-env --list-generations | grep current | grep --only-matching '[0-9]*'"))
		  (output (read-line pipe)))
	 (assert-nix-success (close-pipe pipe))
	 output)))

(define nix-system-services
  (list
   (service nix-service-type)
   (simple-service 'nix-system-packages profile-service-type (packages "nix"))))
