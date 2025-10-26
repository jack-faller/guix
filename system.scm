(define-module (system)
  #:use-module (ice-9 textual-ports)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system privilege)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu system nss)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)

  #:use-module (services iwd)
  #:use-module (utilities)

  #:use-module (ice-9 optargs))
(use-service-modules desktop ssh dbus shepherd avahi pm cups sound sysctl
                     admin linux networking docker)
(use-package-modules vim shells ssh version-control wm linux libusb nfs
                     package-management firmware dns admin admin)

(define*-public (make-system name swap-devices file-systems services packages
                             #:optional #:key
                             (kernel linux)
                             (kernel-arguments '())
                             (grub-theme (grub-theme))
                             (grub-entries '())
                             (extra-users '()))
  (operating-system
    (kernel kernel)
    (kernel-arguments kernel-arguments)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (host-name name)
    (timezone "Europe/London")
    (locale "en_GB.utf8")

    (keyboard-layout (keyboard-layout "gb"))

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)
                 (theme grub-theme)
                 (menu-entries grub-entries)))
    (swap-devices swap-devices)
    (file-systems (cons %fuse-control-file-system
                        (append file-systems %base-file-systems)))
    (services services)
    (packages packages)

    (users (cons* (user-account
                   (name "jack")
                   (comment "Jack")
                   (group "users")
                   (shell (file-append fish "/bin/fish"))
                   (home-directory "/home/jack")
                   (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime" "docker")))
                  (append
                   extra-users
                   %base-user-accounts)))
    (groups (cons*
             ;; Realtime audio.
             (user-group (name "realtime"))
             %base-groups))
    (sudoers-file
     (computed-file
      "sudoers"
      #~(with-output-to-file #$output
          (lambda ()
            (use-modules (ice-9 textual-ports))
            (display (call-with-input-file #$%sudoers-specification
                       get-string-all))
            (newline)
            (display "%wheel ALL=(ALL) NOPASSWD: ")
            (display
             (string-join
              (map
               (lambda (shepherd)
                 (string-append shepherd "/sbin/halt, " shepherd "/sbin/reboot"))
               '(#$shepherd #$(specification->package "shepherd")))
              ", "))
            (newline)))))

    (privileged-programs
     ;; Allow desktop users to also mount NTFS and NFS file systems
     ;; without root.
     (append (map (lambda (program)
                    (privileged-program (program program) (setuid? #t)))
                  (list (file-append nfs-utils "/sbin/mount.nfs")
                        (file-append ntfs-3g "/sbin/mount.ntfs-3g")))
             %default-privileged-programs))
    (name-service-switch %mdns-host-lookup-nss)))

(define-public system-packages
  (cons* vim git fish %base-packages))

(define-public system-services
  ((λ args
     (append
      args
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
           (cons* (f "signing-key.pub") %default-authorized-guix-keys))
          (channels
           (cons*
            (channel
             (name 'nonguix)
             (url "https://gitlab.com/nonguix/nonguix")
             (introduction
              (make-channel-introduction
               "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
               (openpgp-fingerprint
                "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
            %default-channels))))
        (sysctl-service-type
         config =>
         (sysctl-configuration
          (settings (append '(("kernel.sysrq" . "1"))
                            %default-sysctl-settings)))))))
   fontconfig-file-system-service
   (service kernel-module-loader-service-type '("fuse"))
   (service containerd-service-type)
   (service docker-service-type)
   (simple-service 'docker-rotlog log-rotation-service-type
                   '("/var/log/docker.log" "/var/log/containerd.log"))
   (simple-shepherd-service
    'populate-blocklists
    (shepherd-service
     (documentation "Add IPs to blocklists")
     (provision '(populate-blocklists))
     (requirement '(nftables networking))
     (one-shot? #t)
     (start
      #~(make-forkexec-constructor
         (list #$(script-with-path
                  (list inetutils (gexp-input isc-bind "utils") nftables)
                  "populat-blocklists.real" (f "populate-blocklists.sh")))
         #:log-file "/var/log/populate-bocklists.log"))
     (stop #~(make-kill-destructor)))
    '("/var/log/populate-bocklists.log"))
   (service nftables-service-type
            (nftables-configuration (ruleset (f "nftables.conf"))))
   (service iwd-service-type)
   (service connman-service-type
            (connman-configuration
             (shepherd-requirement '(iwd))
             (disable-vpn? #t)
             (general-configuration
              (connman-general-configuration
               (preferred-technologies '("ethernet" "wifi"))
               (network-interface-blacklist
                '("vmnet" "vboxnet" "virbr" "ifb" "docker" "veth" "eth" "wlan"))))))
   (simple-service 'connman-rotlog log-rotation-service-type
                   '("/var/log/connman.log"))
   (service ntp-service-type)
   (service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)
             (authorized-keys
              (let ((my-key (f "myKey.pub")))
                `(("root" ,my-key)
                  ("jack" ,my-key))))))
   (service cups-service-type (cups-configuration (web-interface? #t)))
   (service tlp-service-type)
   (simple-service 'my-udev-rules udev-service-type
                   (list libmtp brightnessctl qmk-udev-rules))

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

   (service sane-service-type)))
