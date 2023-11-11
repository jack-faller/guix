(define-module (packages miny)
  #:export (miny)

  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gl))

(use-modules)

(define commit "6f028ca")
(define revision "5")
(define miny
  (package
   (name "miny")
   (version (git-version "0.6.0" revision commit))
   (source (origin
			(method git-fetch)
			(uri (git-reference
				  (url "https://github.com/jack-faller/miny")
				  (commit commit)))
			(file-name (git-file-name name version))
			(sha256
			 (base32
			  "0qzd9k5xxcgv8lfd2n42g339lj0q30pyyxzqib8qakkfyf8a3h36"))))
   (build-system gnu-build-system)
   (arguments `(#:phases
				(modify-phases
				 %standard-phases
				 (delete 'configure)
				 (replace 'install
						  (lambda* (#:key outputs #:allow-other-keys)
							(let* ((out (assoc-ref outputs "out"))
								   (bin (string-append out "/bin")))
							  (install-file "miny" bin)))))
				#:tests? #f))
   (inputs (list freeglut))
   (home-page "https://github.com/spacecamper/miny")
   (synopsis "Minesweeper")
   (description "Minesweeper")
   (license license:expat)))
miny
