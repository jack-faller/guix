;; this should work from either a guix command or from a load
(or
 (and (current-filename) (dirname (current-filename)))
 (car (filter (lambda (dir)
				(file-exists? (string-append dir "/" (module-filename (current-module)))))
			  %load-path)))
