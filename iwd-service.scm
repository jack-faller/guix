(define-module (iwd-service)
  #:export (iwd-configuration
			iwd-configuration?
			iwd-configuration-iwd
			iwd-configuration-config-file
			iwd-service-type)

  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages dns)
  #:use-module (gnu services admin)
  #:use-module (gnu services dbus)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services))

(use-modules (gnu))

(define-record-type* <iwd-configuration>
  iwd-configuration make-iwd-configuration
  iwd-configuration?
  (iwd iwd-configuration-iwd (default iwd))
  ;; Might be more sensible to set this to a file that works:
  ;; [General]
  ;; EnableNetworkConfiguration=true
  ;; [Network]
  ;; NameResolvingService=resolvconf
  (config-file iwd-configuration-config-file (default #f)))

(define (iwd-shepherd-service config)
  (define environment #~(list (string-append
							   "PATH=" (string-append #$openresolv "/sbin")
							   ":" (string-append #$coreutils "/bin"))))
  (match-record config <iwd-configuration> (iwd)
    (list (shepherd-service
           (documentation "Run Iwd")
           (provision '(iwd networking))
           (requirement '(user-processes dbus-system loopback))
           (start #~(make-forkexec-constructor
                     (list (string-append #$iwd "/libexec/iwd"))
                     #:log-file "/var/log/iwd.log"
					 #:environment-variables #$environment))
           (stop #~(make-kill-destructor))))))

(define %iwd-log-rotation
  (list (log-rotation
         (files '("/var/log/iwd.log")))))

(define (iwd-etc-service config)
  (match-record config <iwd-configuration> (config-file)
     (if config-file
		 `(("iwd/main.conf" ,config-file))
		 '())))

(define iwd-service-type
  (let ((iwd-package (compose list iwd-configuration-iwd)))
	(service-type (name 'iwd)
				  (extensions
				   (list (service-extension shepherd-root-service-type
											iwd-shepherd-service)
						 (service-extension dbus-root-service-type
											iwd-package)
						 (service-extension profile-service-type
											iwd-package)
						 (service-extension etc-service-type
											iwd-etc-service)
						 (service-extension rottlog-service-type
											(const %iwd-log-rotation))))
				  (default-value (iwd-configuration))
				  (description
				   "Run @url{https://iwd.wiki.kernel.org/,Iwd},
a network connection manager."))))
