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
 (list (swap-space
        (target (uuid
                 "a6457cc6-8a1c-4c6c-a825-fc0acc7a60a5"))))
 (cons* (file-system
          (mount-point "/boot/efi")
          ;; /dev/sda1
          (device (uuid "4FEA-894E"
                        'fat32))
          (type "vfat"))
        (file-system
          (mount-point "/")
          ;; /dev/sda2
          (device (uuid
                   "c6053c3f-e0b2-4eb6-9d85-db0f4912a1be"
                   'ext4))
          (type "ext4"))
        (let ((hdd
               (file-system
                 (mount-point "/hdd")
                 ;; /dev/sdb5
                 (device (uuid
                          "89e1f679-1de8-4294-b636-4342260bf3ec"
                          'ext4))
                 (type "ext4"))))
          (cons*
           hdd
           (file-system
             (mount-point "/home")
             (device "/hdd/home")
             (type "none")
             (flags '(bind-mount))
             (dependencies (list hdd)))
           %base-file-systems)))
 (cons* (service nvidia-service-type) system-services)
 (using-nvidia system-packages)
 #:kernel linux-lts
 #:kernel-arguments
 (cons*
  "modprobe.blacklist=nouveau"
   ;; Set this if the card is not used for displaying or
   ;; you're using Wayland:
   "nvidia_drm.modeset=1"
   %default-kernel-arguments))
