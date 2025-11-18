(define-module (configuration ydotool)
  #:export (ydotool-system-services ydotool-home-services)
  #:use-module (utilities)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg))

(define ydotool-system-services
  (list
   (simple-shepherd-service
    'ydotool-daemon
    (shepherd-service
     (documentation "Run ydotoold")
     (provision '(ydotoold))
     (requirement '(udev))
     (start
      #~(make-forkexec-constructor
         (list #$(file-append ydotool "/bin/ydotoold")
               "--socket-path=/tmp/.ydotool_socket"
               "--socket-perm=0600"
               "--socket-own=root:root")
         #:log-file "/var/log/ydotoold.log"))
     (stop #~(make-kill-destructor)))
    '("/var/log/ydotoold.log"))))

;; Need to remove /etc/ld.so.cache as it conflicts with easyeffects.
(define ydotool-no-ld.so.cache
  (package
    (inherit ydotool)
    (source ydotool)
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("bin" "bin")
          ("share" "share"))))))
(define ydotool-home-services
  (list
   (simple-service 'ydotool-environment-variables
                   home-environment-variables-service-type
                   '(("YDOTOOL_SOCKET" . "/tmp/.ydotool_socket")))
   ;; For some reason, the ydotool package breaks the environment.
   (simple-service 'ydotool-profile home-profile-service-type (list ydotool-no-ld.so.cache))))
