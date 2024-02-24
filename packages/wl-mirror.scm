(define-module (packages wl-mirror)
  #:export (wl-mirror)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages image)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages man)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config))

(define wlr-protocols
  (let ((commit "2b8d4332")
        (revision "0"))
    (package
      (name "wlr-protocols")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/wlroots/wlr-protocols")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17blwww6rcrahwc6h6j68gh6wjbj14if3mihpxymfdw5pwl72rav"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (use-modules (guix build utils))
                        (let* ((out (assoc-ref outputs "out"))
                               (share (string-append out "/share")))
                          (mkdir-p share)
                          (setenv "datarootdir" share)
                          (invoke "make" "install"
                                  (string-append "PREFIX=" out)))))
                    (delete 'configure))))
      (native-inputs (list wayland))
      (home-page "https://gitlab.freedesktop.org/wlroots/wlr-protocols")
      (synopsis
       "Wayland protocols designed for use in wlroots (and other compositors)")
      (description
       "Wayland protocols designed for use in wlroots (and other compositors)")
      (license license:expat))))

(define wl-mirror
  (package
    (name "wl-mirror")
    (version "0.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Ferdi265/wl-mirror")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0464m60xsbpfwvsszrdkjsxfvrbkr71hp7phsz05cqyvjwf6cism"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DINSTALL_DOCUMENTATION=ON"
                                (string-append "-DWL_PROTOCOL_DIR="
                                               #$wayland-protocols
                                               "/share/wayland-protocols")
                                (string-append "-DWLR_PROTOCOL_DIR="
                                               #$wlr-protocols
                                               "/share/wlr-protocols"))
      #:tests? #f))
    (native-inputs (list pkg-config scdoc))
    (inputs (list slurp
                  wayland
                  mesa
                  eglexternalplatform
                  wayland-protocols
                  sway
                  wlr-protocols))
    (home-page "https://github.com/Ferdi265/wl-mirror")
    (synopsis "Mirror an output to a window")
    (description
     "Wl-mirror attempts to provide a solution to sway's lack of output mirroring by mirroring an output onto a client surface.")
    (license license:gpl3+)))
wl-mirror
