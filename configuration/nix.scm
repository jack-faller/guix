(define-module (configuration nix)
  #:export (nix-packages-service-type nix-system-services)

  #:use-module (utilities)

  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu services nix))

(define nix-packages-service-type
  (service-type
   (name 'nix-packages)
   (compose concatenate)
   (extend append)
   (description
    "Make sure the given nix packages are installed, remove all other packages and install only them if they are not")
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
        #~(begin
	    (define packages '#$packages)
	    (define (invoke . args)
	      (unless (= 0 (apply system* args))
	        (error "failed to run nix command" args)))
	    (setenv "NIXPKGS_ALLOW_UNFREE" "1")
	    (invoke #$(shell-script
		       "nix-add-channels"
		       "if [ \"$(nix-channel --list)\" = \"\" ]; then"
		       "  nix-channel --add https://nixos.org/channels/nixpkgs-unstable || exit 1"
		       "  nix-channel --update || exit 1"
		       "  nix-env -iA nixpkgs.nix nixpkgs.cacert || exit 1"
		       "fi"))
	    ;; Check that all packages are installed and that no additional
	    ;; packages are installed.
	    (when (or (not (= 0 (apply system* "nix-env" "--query" "--installed"
				       packages)))
		      (not (= 0 (system
			         (string-append
				  "test " (number->string (length packages))
				  " = $(nix-env --query | wc -l)")))))
	      (apply invoke "nix-env" "--remove-all" "--install" packages)))))))))

(define nix-system-services
  (list
   (service nix-service-type)
   (simple-service 'nix-system-packages profile-service-type (list (specification->package "nix")))))
