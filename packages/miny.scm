(use-modules (guix packages)
			 (guix git-download)
			 (guix build-system gnu)
			 ((guix licenses) #:prefix license:)
			 (gnu packages gl))

(define miny
  (package
   (name "miny")
   (version "0.5.12")
   (source (origin
			(method git-fetch)
			(uri (git-reference
				  (url "https://github.com/spacecamper/miny")
				  (commit "4f6b214")))
			(file-name (git-file-name name version))
			(sha256
			 (base32
			  "02c8dqiqk9j818daqmkc68clxgs0bdsr8g7xfywp6pjsahjc1byg"))))
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
