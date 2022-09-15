(use-modules (ice-9 textual-ports)
             (guix gexp)
             (gnu services)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu packages vim)
             (gnu packages web)
             (gnu packages bash)
             (gnu packages admin)
			 (gnu packages version-control)
			 (gnu packages password-utils)
			 (gnu packages wm))

(use-modules (ice-9 textual-ports))
(load (canonicalize-path "./wants.scm"))

(home-environment
 (packages
  (list
   sway
   git
   vim
   password-store
   emacs))
 (services
  (list
   (service home-zsh-service-type (home-zsh-configuration)))))
