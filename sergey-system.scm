(add-to-load-path (dirname (current-filename)))
(use-modules (ice-9 textual-ports)
			 (gnu)
			 (gnu services)
			 (gnu system setuid)
			 (guix gexp)
			 (gnu system nss)
			 (nongnu system linux-initrd)
			 (nongnu packages linux)

			 (services networking)
			 (configuration nix)
			 (configuration sway-desktop)

			 ((gnu services networking)
			  #:select (ntp-service-type)))

(use-service-modules desktop ssh dbus shepherd avahi pm
					 cups sound sysctl)
(use-package-modules vim shells ssh version-control wm linux libusb nfs
					 package-management)

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (host-name "sergey")
 (timezone "Europe/London")
 (locale "en_GB.utf8")

 (keyboard-layout (keyboard-layout "gb"))

 (bootloader (bootloader-configuration
			  (bootloader grub-efi-bootloader)
			  (targets (list "/boot/efi"))
			  (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
					  (target (uuid
							   "bdef5cf4-2afd-4766-b0fc-0acd63e3ab52")))))

 (file-systems (cons* (file-system
					   (mount-point "/boot/efi")
					   (device (uuid "6AF7-DD2E"
									 'fat32))
					   (type "vfat"))
					  (file-system
					   (mount-point "/")
					   (device (uuid
								"cabea022-7117-40c7-b90c-f6b5a5b7a0c8"
								'ext4))
					   (type "ext4"))
					  (file-system
					   (mount-point "/home")
					   (device (uuid
								"14f8df88-3802-40f7-9e29-ec0a89a69664"
								'ext4))
					   (type "ext4")) %base-file-systems))

 (users (cons* (user-account
				(name "jack")
				(comment "Jack")
				(group "users")
				(shell (file-append zsh "/bin/zsh"))
				(home-directory "/home/jack")
				(supplementary-groups '("wheel" "netdev" "audio" "video")))
			   %base-user-accounts))
 (sudoers-file
  (computed-file
   "sudoers"
   #~(with-output-to-file #$output
	   (lambda ()
		 (use-modules (ice-9 textual-ports))
		 (display (call-with-input-file #$%sudoers-specification
										get-string-all))
		 (newline)
		 (define str "%wheel ALL=(ALL) NOPASSWD: /home/jack/.guix-home/profile/sbin/")
		 (define (nopass prog) (display str) (display prog) (newline))
		 (nopass "halt")
		 (nopass "reboot")))))

 (packages (cons* vim git zsh (specification->package "nss-certs") %base-packages))

 (name-service-switch %mdns-host-lookup-nss)

 (services
  ((λ args
	 (append
	  args
	  sway-desktop-system-services
	  nix-system-services
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
		  (cons* (local-file "/config/signing-key.pub")
				 %default-authorized-guix-keys))))
	   (sysctl-service-type
		config =>
		(sysctl-configuration
		 (settings (append '(("kernel.sysrq" . "1"))
						   %default-sysctl-settings)))))))

   ;; Allow desktop users to also mount NTFS and NFS file systems
   ;; without root.
   (simple-service 'mount-setuid-helpers setuid-program-service-type
                   (map (lambda (program)
                          (setuid-program
                           (program program)))
						(list (file-append nfs-utils "/sbin/mount.nfs")
                              (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))
   fontconfig-file-system-service
   (service iwd-service-type)
   (service connman-service-type
			(connman-configuration
			 (main
			  '((General
				 ((AllowHostNameUpdates . #f)
				  (AllowDomainNameUpdates .#f)
				  (PreferredTechnologies
				   ethernet wifi)
				  (NetworkInterfacesBlacklist
				   vmnet vboxnet virbr ifb docker veth eth wlan)))))))
   (service ntp-service-type)
   (service openssh-service-type
			(openssh-configuration
			 (permit-root-login 'prohibit-password)
			 (authorized-keys
			  (let ((my-key (local-file "myKey.pub")))
				`(("root" ,my-key)
				  ("jack" ,my-key))))))
   (service cups-service-type)
   (service tlp-service-type)
   (simple-service 'my-udev-rules udev-service-type (list libmtp brightnessctl))

   ;; The D-Bus clique.
   (service avahi-service-type)
   (service udisks-service-type)
   (service upower-service-type)
   (service accountsservice-service-type)
   (service cups-pk-helper-service-type)
   (service colord-service-type)
   (service geoclue-service-type)
   (service polkit-service-type)
   polkit-wheel-service
   (service elogind-service-type)
   (service dbus-root-service-type)

   (service sane-service-type))))
