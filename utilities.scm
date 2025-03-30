(define-module (utilities)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (srfi srfi-26)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu home services)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd))

;; make this work with both guix pull and from other commands
(define*-public config-directory (current-source-directory))
(define*-public (f . x)
  (define name (apply string-append config-directory "/files/" x))
  (unless (file-exists? name)
    (error "File doesn't exist:" name))
  (local-file name #:recursive? #t))
(define*-public (not-dot file) (not (or (string= file ".") (string= file ".."))))
(define*-public (lines . lines) (string-join lines "\n" 'suffix))

(define*-public (shell-script name . lines)
  (apply script name "#!/bin/sh" lines))
(define*-public (script name . lines)
  (computed-file
   name
   #~(begin
       (use-modules (ice-9 ports))
       (define lines (list #$@lines))
       (call-with-output-file #$output
	 (lambda (file)
           (for-each (lambda (line) (display line file) (newline file))
                     lines)))
       (chmod #$output #o555))))

(define*-public (simple-shepherd-service name shepherd-service #:optional log-files)
  (define shepherd-extension
    (service-extension shepherd-root-service-type
                       (const (list shepherd-service))))
  (define extensions
    (if log-files
        (list shepherd-extension
              (service-extension log-rotation-service-type (const log-files)))
        (list shepherd-extension)))
  (service (service-type (name name) (extensions extensions) (description "Run a shepherd service and rotate its logs")) #f))

(define*-public (script-with-path packages name file)
  (use-modules (gnu packages base))
  (define ps (cons* coreutils grep sed packages))
  (program-file
   name
   #~(let ((bins '(#$@(map (cut file-append <> "/bin") ps)
                   #$@(map (cut file-append <> "/sbin") ps))))
       (setenv "PATH" (string-join bins ":"))
       (apply execlp #$file (cdr (program-arguments))))))

(define*-public (using-nvidia packages)
  (map (lambda (a)
         (if (list? a)
             (cons (replace-mesa (car a)) (cdr a))
             (replace-mesa a)))
       packages))

(define*-public (rust-script name file)
  (computed-file
   name
   (with-imported-modules '((guix build utils))
     #~(let ((gcc #$(@ (gnu packages commencement) gcc-toolchain))
             (rust #$(@ (gnu packages rust) rust)))
         (use-modules (guix build utils))
         (setenv "PATH" (string-append rust "/bin:" gcc "/bin:" (getenv "PATH")))
         (setenv "LIBRARY_PATH" (string-append gcc "/lib:" rust "/lib"))
         (invoke "rustc" "-O" #$file "-o" #$output)))))

(define*-public (c-script name file)
  (computed-file
   name
   (with-imported-modules '((guix build utils))
     #~(let ((gcc #$(@ (gnu packages commencement) gcc-toolchain)))
         (use-modules (guix build utils))
         (setenv "PATH" (string-append gcc "/bin:" (getenv "PATH")))
         (setenv "LIBRARY_PATH" (string-append gcc "/lib"))
         (setenv "C_INCLUDE_PATH"
                 (string-append
                  #$(@ (gnu packages linux) linux-libre-headers) "/include"))
         (invoke "gcc" "-O2" #$file "-o" #$output)))))

(define*-public (specifications->package-list . names)
  (map (lambda (i)
	 (cond
	  ((string? i)
	   (if (string-contains i ":")
	       (receive (a b) (specification->package+output i) (list a b))
	       (specification->package i)))
	  ((and (list? i) (= (length i) 2) (string? (cadr i)))
	   (if (string? (car i))
	       (cons (specification->package (car i)) (cdr i))
	       i))
	  ((package? i) i)
	  (else
	   (error "Unrecognised package format"))))
       names))
