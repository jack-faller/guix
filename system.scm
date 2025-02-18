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

  #:use-module (services networking)
  #:use-module (configuration nix)
  #:use-module (utilities)

  #:use-module (ice-9 optargs)

  #:use-module ((gnu services networking)
                #:select (ntp-service-type)))

(use-service-modules desktop ssh dbus shepherd avahi pm cups sound sysctl
                     admin linux)
(use-package-modules vim shells ssh version-control wm linux libusb nfs
                     package-management firmware)

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
                   (shell (file-append zsh "/bin/zsh"))
                   (home-directory "/home/jack")
                   (supplementary-groups '("wheel" "netdev" "audio" "video" "realtime" "lp" "docker")))
                  (append
                   extra-users
                   %base-user-accounts)))
    (groups (cons*
             ;; Realtime audio.
             (user-group (name "realtime"))
             (user-group (name "docker"))
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
            (define str "%wheel ALL=(ALL) NOPASSWD: /home/jack/.guix-home/profile/sbin/")
            (define (nopass prog) (display str) (display prog) (newline))
            (nopass "halt")
            (nopass "reboot")))))

    (privileged-programs
     ;; Allow desktop users to also mount NTFS and NFS file systems
     ;; without root.
     (append (map (lambda (program)
                    (privileged-program (program program) (setuid? #t)))
                  (list (file-append nfs-utils "/sbin/mount.nfs")
                        (file-append ntfs-3g "/sbin/mount.ntfs-3g")))
             %default-privileged-programs))
    (name-service-switch %mdns-host-lookup-nss)))

(define-public system-packages (cons* vim git zsh %base-packages))

(define-public system-services
  ((λ args
     (append
      args
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
            (channel (name 'rde)
                     (url "https://git.sr.ht/~abcdw/rde")
                     (introduction
                      (make-channel-introduction
                       "257cebd587b66e4d865b3537a9a88cccd7107c95"
                       (openpgp-fingerprint
                        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))

            %default-channels))))
        (sysctl-service-type
         config =>
         (sysctl-configuration
          (settings (append '(("kernel.sysrq" . "1"))
                            %default-sysctl-settings)))))))
   fontconfig-file-system-service
   (service kernel-module-loader-service-type '("fuse"))
   (simple-service
    'udevmon-rotlog log-rotation-service-type
    '("/var/log/udevmon.log"))
   (simple-service
    'udevmon shepherd-root-service-type
    (list
     (shepherd-service
      (documentation "Run udevmon")
      (provision '(udevmon))
      (requirement '(user-processes udev))
      (start #~(make-forkexec-constructor
                (list
                 #$(file-append coreutils "/bin/nice")
                 "-n" "-20"
                 #$(file-append interception-tools "/bin/udevmon")
                 "-c"
                 #$(computed-file
                    "udevmon.yaml"
                    (with-imported-modules '((guix build utils))
                      #~(begin
                          (use-modules (guix build utils))
                          (copy-file #$(f "udevmon.yaml") #$output)
                          (chmod #$output #o555)
                          (substitute* #$output
                            (("HOMEROW_MOD")
                             #$(c-script "homerow-mod" (f "homerow-mod.c")))
                            (("INTERCEPTION_BIN")
                             #$(file-append interception-tools "/bin")))))))
                #:log-file "/var/log/udevmon.log"))
      (stop #~(make-kill-destructor)))))
   (service pam-limits-service-type
            (list
             (pam-limits-entry "@realtime" 'both 'rtprio 99)
             (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
   (service iwd-service-type)
   (service connman-service-type
            (connman-configuration
             (main
              '((General
                 ((PreferredTechnologies
                   ethernet wifi)
                  (NetworkInterfacesBlacklist
                   vmnet vboxnet virbr ifb docker veth eth wlan)))))))
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
