(define-module (packages remote-packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (remote-package)
  #:export (miny jaq))

(define miny
  (let ((release "0.6.0") (commit "a60e8e4") (revision "20"))
    (package
      (name "miny")
      (source (remote-package
               (source
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/jack-faller/miny")
                        (commit commit)))
                  (file-name (git-file-name name (string-append release "-" commit "-" revision)))
                  (sha256
                   (base32 "0rmaf7y28az4c793rlg463h95scczyw2q5r0giq3zg2i0fc3j459"))))))
      (version (git-version release revision commit))
      (build-system (@ (guix build-system copy) copy-build-system))
      (synopsis "Minesweeper")
      (description "Minesweeper")
      (home-page "https://github.com/spacecamper/miny")
      (license license:expat))))

(define jaq
  (let ((release "3.0.0-beta") (commit "a19989f") (revision "1"))
    (package
      (name "jaq")
      (source (remote-package
               (source
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/jack-faller/jaq")
                        (commit commit)))
                  (file-name (git-file-name name (string-append release "-" commit "-" revision)))
                  (sha256
                   (base32 "1x8yfk6yxdvxp532r7lbnzhnrbh1qqvrqmi6zqdlis3r83gs3ycj"))))))
      (version (git-version release revision commit))
      (build-system (@ (guix build-system copy) copy-build-system))
      (synopsis "An interpreter for the jq programming language.")
      (description "Jaq (pronounced /ʒaːk/, like Jacques) is a clone of the JSON data processing tool jq.  It has a few features not present in jq, such as support for the data formats YAML, CBOR, TOML, and XML.")
      (home-page "https://gedenkt.at/jaq/manual/")
      (license license:expat))))
