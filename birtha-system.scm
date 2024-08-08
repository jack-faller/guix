(add-to-load-path (dirname (current-filename)))
(use-modules (gnu)
             (gnu services)
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
          (cons*
           hdd
	   arch
	   (bind "/ssd" "/arch/ssd" arch)
	   (bind "/home" "/hdd/home" hdd)
	   %base-file-systems)))
 (cons* (service nvidia-service-type) system-services)
 (using-nvidia system-packages)
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
   (device "SSDARCHEFI")
   (chain-loader "(hd1,4)/EFI/GRUB/grubx64.efi"))))
