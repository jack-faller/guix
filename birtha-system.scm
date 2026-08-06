(add-to-load-path (dirname (current-filename)))
(use-modules (gnu)
             (guix gexp)
             (gnu services)
             (gnu services admin)
             (gnu services docker)
             (gnu services xorg)
             (gnu packages shells)
             (gnu services shepherd)
             (gnu packages base)
             ;; TODO: AMD container toolkit.
             ;; (packages nvidia-container-toolkit)
             (utilities)
             (configuration sway-desktop)
             (system)
             (nongnu packages linux)
             (nonguix transformations))

(define generic-system
  (make-system
   "birtha"
   (cons*
    (file-system
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
       (bind "/home" "/hdd/home" hdd))))))

(operating-system
  (inherit generic-system)
  (swap-devices (list (swap-space (target (file-system-label "HDD-SWAP")))))
  ;; Need to put this here or gets added two times.
  (essential-services '())
  (services (append sway-desktop-system-services
                    (operating-system-services generic-system)))
  (packages (cons* amdgpu-firmware
                   (operating-system-packages generic-system)))
  (bootloader
   (let ((parent (operating-system-bootloader generic-system)))
     (bootloader-configuration
      (inherit parent)
      (menu-entries
       (cons (menu-entry
              (label "Arch")
              ;; I think there is a bug that stops this from working, so must qualify (hd0,4) below.
              (device "SSDARCHEFI")
              ;; Could have put these both in the same partition with a different label.
              ;; I think I did this by accident anyway at some point.
              (chain-loader "(hd0,4)/EFI/GRUB/grubx64.efi"))
             (bootloader-configuration-menu-entries parent)))))))
