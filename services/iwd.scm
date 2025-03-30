;; copied from krevedkokun's config
(define-module (services iwd)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix import utils)

  #:use-module (gnu packages dns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages networking)

  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)

  #:use-module ((gnu home-services-utils)
                #:select (ini-config?
                          maybe-object->string
                          generic-serialize-ini-config))
  #:export (iwd-service-type iwd-configuration))

(define (serialize-ini-config config)
  (define (serialize-val val)
    (cond
     ((boolean? val) (if val "true" "false"))
     ((list? val) (string-join (map serialize-val val) ","))
     ((or (number? val) (symbol? val)) (maybe-object->string val))
     (else val)))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      (list key " = " val "\n")))

  (generic-serialize-ini-config
   #:combine-ini (compose flatten interpose)
   #:combine-alist list
   #:combine-section-alist cons
   #:serialize-field serialize-field
   #:fields config))

(define-configuration/no-serialization iwd-configuration
  (package (package iwd) "")
  (config (ini-config '()) ""))

(define (iwd-shepherd-service cfg)
  (match-record cfg <iwd-configuration>
    (package)
    (let ((environment #~(list (string-append
                                "PATH="
                                (string-append #$openresolv "/sbin")
                                ":"
                                (string-append #$coreutils "/bin")))))
      (list
       (shepherd-service
        (documentation "Run iwd")
        (provision '(iwd))
        (requirement '(user-processes dbus-system loopback))
        (start #~(make-forkexec-constructor
                  (list (string-append #$package "/libexec/iwd"))
                  #:log-file "/var/log/iwd.log"
                  #:environment-variables #$environment))
        (stop #~(make-kill-destructor)))))))

(define (iwd-etc-service cfg)
  (match-record cfg <iwd-configuration>
    (config)
    `(("iwd/main.conf"
       ,(apply mixed-text-file
               "main.conf"
               (serialize-ini-config config))))))

(define iwd-log-rotation '("/var/log/iwd.log"))

(define add-iwd-package (compose list iwd-configuration-package))

(define iwd-service-type
  (service-type
   (name 'iwd)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           iwd-shepherd-service)
          (service-extension
           dbus-root-service-type
           add-iwd-package)
          (service-extension
           etc-service-type
           iwd-etc-service)
          (service-extension
           profile-service-type
           add-iwd-package)
	  (service-extension
	   log-rotation-service-type
	   (const iwd-log-rotation))))
   (default-value (iwd-configuration))
   (description "")))
