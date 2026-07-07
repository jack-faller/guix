(define-module (remote-package)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (remote-package
            remote-package? remote-package-source remote-package-package-file
            remote-package-replace-source?
            git-clean-file? git-local-tree
            update-lock)
  #:re-export (origin base32))

(define (git-clean-file? file stat)
  "For use in #:select? argument of LOCAL-FILE, return if file is not .git or *.scm.lock and not git ignored."
  (define old-cwd (getcwd))
  (chdir (dirname file))
  (define keep?
    (and (not (equal? (basename file) ".git"))
	 (with-output-to-file "/dev/null"
	   (lambda ()
	     (= 1 (status:exit-val (system* "git" "check-ignore" file)))))))
  (chdir old-cwd)
  keep?)
(define (regexp-not-matches? prefix regexp)
  "Return a function for the #:select? argument of LOCAL-FILE that excludes files matching REGEXP, stripping PREFIX from the file's path before matching."
  (lambda (file stat)
    (not (string-match regexp (if (string-prefix? prefix file)
                                  (substring file (string-length prefix))
                                  file)))))
(define* (git-local-tree directory #:optional #:key (exclude "[^/]*\\.scm\\.lock"))
  "Take the non-git-ignored working tree from DIRECTORY, excluding any file matching the regexp EXCLUDE."
  (local-file directory #:recursive? #t
              #:select? (lambda (file stat)
                          (or (git-clean-file? file stat)
                              ((regexp-not-matches? directory exclude) file stat)))))

(define-record-type* <remote-package>
  remote-package make-remote-package remote-package? this-remote-package
  (source remote-package-source)
  ;; Can be either a string, in which case the result is from loading the
  ;; file, or a list of '(@ (path to module) variable) in which case as
  ;; `variable` is used from the given module.
  (package-location remote-package-package-location (default "guix.scm"))
  (replace-source? remote-package-replace-source? (default #t)))

(define (lowered-and-built object system target)
  (mlet %store-monad ((lowered (lower-object object system #:target target)))
    (cond
     ((string? lowered)
      (return lowered))
     ;; This fixes a bug where the file interpolated into the gexp is not the
     ;; same one as exposed in its inputs.
     ;; I'm not sure why that happens or why this is the correct solution.
     ((file-exists? (derivation->output-path lowered))
      (return (derivation->output-path lowered)))
     (else
      (mbegin %store-monad
        (built-derivations (list lowered))
        (return (derivation->output-path lowered)))))))

(define-gexp-compiler (remote-package-compiler
                       (this <remote-package>) system target)
  (define source (remote-package-source this))
  (define location (remote-package-package-location this))
  (mlet* %store-monad ((dir (lowered-and-built source system target)))
    (let* ((old-path %load-path)
           (old-cwd (getcwd)))
      (chdir dir)
      (set! %load-path (cons dir %load-path))
      (define loaded
        (match location
          (('@ module variable) (module-ref (resolve-module module) variable))
          ((? string?) (load location))
          (_ (error "Invalid file location of remote package" this location))))
      (define loaded*
        (if (and (package? loaded) (remote-package-replace-source? this))
            (package (inherit loaded) (source source))
            loaded))
      (set! %load-path old-path)
      (chdir old-cwd)
      (lower-object loaded* system #:target target))))

(define* (update-lock
          arg0 #:optional (file "guix.scm") (lock-file "guix-channels.scm"))
  "Build a package file and create a lock file containing the channel versions used in the build."
  (guix-main "guix" "build" "-f" file)
  (with-output-to-file lock-file
    (lambda () (guix-main "guix" "describe" "--format=channels"))))

;; TODO: Add importer for remote packages.
;; guix import remote-package miny https://github.com/jack-faller/miny --git-commit=a60e8e4
