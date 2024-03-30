(define-module (packages miny)
  #:export (miny)

  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages gl))

(define miny
  (let ((commit "6ad92b8")
        (revision "8"))
    (package
      (name "miny")
      (version (git-version "0.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jack-faller/miny")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gf9bnrsqnswmlp6z1rmbpyp1i7xxvnw5cyy8b6kdd8ymr1s1dc2"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
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
      (license license:expat))))
miny
