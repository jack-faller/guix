(define-module (utilities)
  #:use-module (gnu)
  #:use-module (gnu services)
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
  #:use-module (ice-9 string-fun))

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
