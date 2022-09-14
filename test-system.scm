;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (ice-9 textual-ports)
			 (gnu)
			 (gnu packages vim)
			 (gnu packages ssh)
             (gnu packages version-control)
             (gnu services ssh)
             (gnu services networking)
			 (nongnu packages linux)
			 (nongnu system linux-initrd))

(load (canonicalize-path "wants.scm"))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (host-name hostname)
 (timezone "Europe/London")
 (locale "en_GB.utf8")

 (keyboard-layout (keyboard-layout "uk"))

 (bootloader (cond
			  ((equal? hostname "sergey")
			   (bootloader-configuration
				(bootloader grub-efi-bootloader)
				(target "/boot/efi")
				(keyboard-layout keyboard-layout)))
			  ((equal? hostname "mikhail")
			   (bootloader-configuration
				(bootloader grub-bootloader)
				(targets '("/dev/sda"))))
			  (else (error "no file system for host name"))))
 (file-systems (append
				(cond
				 ((equal? hostname "sergey")
				  (list
				   (file-system
					(device "/dev/sda1")
					(mount-point "/boot/efi")
					(type "vfat"))
				   (file-system
					(device "/dev/sda2")
					(mount-point "/")
					(type "ext4"))))
				 ((equal? hostname "mikhail")
				  (list (file-system
						 (device "/dev/sda3")
						 (mount-point "/")
						 (type "ext4"))))
				 (else (error "no file system for host name")))
				%base-file-systems))

 ;; This is where user accounts are specified.  The "root"
 ;; account is implicit, and is initially created with the
 ;; empty password.
 (users (cons (user-account
			   (name "jack")
			   (group "users")
			   (supplementary-groups '("wheel" "audio" "video")))
			  %base-user-accounts))

 ;; Globally-installed packages.
 (packages (cons* vim git %base-packages))

 (services (append (list (service dhcp-client-service-type)
						 (service openssh-service-type
								  (openssh-configuration
								   (openssh openssh-sans-x)
								   (authorized-keys
									`(("jack" ,(local-file "myKey.pub"))
									  ("root" ,(local-file "myKey.pub"))))
								   (port-number 22))))
				   (modify-services
					%base-services
					(guix-service-type
					 config =>
					 (guix-configuration
					  (inherit config)
					  (substitute-urls
					   (append (list "https://substitutes.nonguix.org")
							   %default-substitute-urls))
					  (authorized-keys
					   (append (list (local-file "signing-key.pub"))
							   %default-authorized-guix-keys))))))))
