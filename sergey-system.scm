(add-to-load-path (dirname (current-filename)))
(use-modules (gnu)
             (gnu services)
             (configuration sway-desktop)
             (system))

(define generic-system
  (make-system
   "sergey"
   (list
    (file-system
      (mount-point "/boot/efi")
      (device (uuid "6AF7-DD2E" 'fat32))
      (type "vfat"))
    (file-system
      (mount-point "/")
      (device (uuid "cabea022-7117-40c7-b90c-f6b5a5b7a0c8" 'ext4))
      (type "ext4"))
    (file-system
      (mount-point "/home")
      (device (uuid "14f8df88-3802-40f7-9e29-ec0a89a69664" 'ext4))
      (type "ext4")))))

(operating-system
  (inherit generic-system)
  (host-name "sergey")
  (swap-devices
   (list (swap-space (target (uuid "bdef5cf4-2afd-4766-b0fc-0acd63e3ab52")))))
  ;; Need to put this here or gets added two times.
  (essential-services '())
  (services (append sway-desktop-system-services
                    (operating-system-services generic-system))))
