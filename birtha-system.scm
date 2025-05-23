(add-to-load-path (dirname (current-filename)))
(use-modules (gnu)
             (gnu services)
             (gnu packages shells)
             (utilities)
             (system)
             (gnu services xorg)
             (nongnu packages linux)
             (nongnu packages nvidia)
             (nongnu services nvidia))

(make-system
 "birtha"
 (list (swap-space (target (file-system-label "HDD-SWAP"))))
 (cons* (file-system
          (mount-point "/boot/efi")
          (device (file-system-label "SSD-EFI"))
          (type "vfat"))
        (file-system
          (mount-point "/")
          (device (file-system-label "SSD-GUIX-ROOT"))
          (type "ext4"))
        (let ((hdd
               (file-system
                 (mount-point "/hdd")
                 (device (file-system-label "HDD"))
                 (type "ext4")))
	      (arch
	       (file-system
		 (mount-point "/arch")
		 (device (file-system-label "SSD-ARCH-ROOT"))
		 (type "ext4")))
	      (bind (lambda (to from in)
		      (file-system
		        (mount-point to)
		        (device from)
		        (type "none")
		        (flags '(bind-mount))
		        (dependencies (list in))))))
          (list
           hdd
	   arch
	   (bind "/ssd" "/arch/ssd" arch)
	   (bind "/home" "/hdd/home" hdd))))
 (cons* (service nvidia-service-type) system-services)
 (using-nvidia system-packages)
 #:extra-users
 (list
  (user-account
   (name "steam")
   (comment "Steam")
   (group "users")
   (shell (file-append zsh "/bin/zsh"))
   (home-directory "/home/steam")
   (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime"))))
 #:grub-theme (grub-theme
               (inherit (grub-theme))
               (gfxmode '("1920x1080x32" "1024x786x32" "auto")))
 #:kernel linux-lts
 #:kernel-arguments
 (cons*
  "modprobe.blacklist=nouveau"
  ;; Set this if the card is not used for displaying or
  ;; you're using Wayland:
  "nvidia_drm.modeset=1"
  %default-kernel-arguments)
 #:grub-entries
 (list
  (menu-entry
   (label "Arch")
   ;; I think there is a bug that stops this from working, so must qualify (hd0,4) below.
   (device "SSDARCHEFI")
   ;; Could have put these both in the same partition with a different label.
   ;; I think I did this by accident anyway at some point.
   (chain-loader "(hd0,4)/EFI/GRUB/grubx64.efi"))))
