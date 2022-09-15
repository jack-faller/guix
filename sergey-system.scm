(use-modules (ice-9 textual-ports)
			 (gnu)
			 (gnu packages vim)
			 (gnu packages ssh)
             (gnu packages version-control)
             (gnu services ssh)
             (gnu services networking)
			 (nongnu packages linux)
			 (nongnu system linux-initrd))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (host-name "sergey")
 (timezone "Europe/London")
 (locale "en_GB.utf8")

 (keyboard-layout (keyboard-layout "uk"))

 (bootloader (bootloader-configuration
			  (bootloader grub-efi-bootloader)
			  (target "/boot/efi")
			  (keyboard-layout keyboard-layout)))
 (file-systems (append
				(list
				 (file-system
				  (device "/dev/sda1")
				  (mount-point "/boot/efi")
				  (type "vfat"))
				 (file-system
				  (device "/dev/sda2")
				  (mount-point "/")
				  (type "ext4"))
				 (file-system
				  (device "/dev/sda3")
				  (mount-point "/home")
				  (type "ext4")))
				%base-file-systems))

 (users (cons (user-account
			   (name "jack")
			   (group "users")
			   (supplementary-groups '("wheel" "audio" "video")))
			  %base-user-accounts))

 (packages (cons* vim git %base-packages))

 (services (append
			(list (service dhcp-client-service-type)
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
