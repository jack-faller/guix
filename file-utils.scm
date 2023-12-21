(define-module (file-utils)
  #:export (lines config-directory executable-shell-script config-files-service-type packages f)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun))

;; make this work with both guix pull and from other commands
(define config-directory (load "get-config-dir.scm"))
(define (fname . x) (apply string-append config-directory "/files/" x))
(define (f . x)
  (define name (apply fname x))
  (unless (file-exists? name)
	(error "File doesn't exist:" name))
  (local-file name #:recursive? #t))
(define (not-dot file) (not (or (string= file ".") (string= file ".."))))
(define (lines . lines) (string-join lines "\n" 'suffix))

(define (executable-shell-script name . lines-list)
  (define script (apply lines (cons "#!/bin/sh" lines-list)))
  (computed-file name #~(begin
						  (use-modules (ice-9 ports))
						  (call-with-output-file #$output
							(λ (file) (display #$script file)))
						  (chmod #$output #o555))))

(define (file-pairs target file-raw)
  (define file (fname file-raw))
  (file-system-fold
   (const #t)
   (λ (path stat acc)
	 (cons (list (string-replace-substring path file target)
				 (local-file path #:recursive? #t))
		   acc))
   (λ (_1 _2 acc) acc)
   (λ (_1 _2 acc) acc)
   (const #t)
   (λ (path stat acc)
	 (error "File doesn't exist:" path))
   '()
   file))
(define config-files-service-type
  (service-type
   (name 'config-files)
   (extensions
	(list
	 (service-extension
	  home-files-service-type
	  (λ (files)
		(append-map
		 (lambda (pair)
		   (if (string? (cadr pair))
			   (apply file-pairs pair)
			   (list pair)))
		 files)))))
   (compose concatenate)
   (extend append)
   (description
	"Like home-files-service-type, but when a string is provided, take it recursively from the files directory.")))

(define (packages . names)
  (map (lambda (i) (if (string? i) (specification->package i) i))
	   names))
