(define-module (configuration bspwm-desktop)
  #:export (bspwm-desktop-home-services)

  #:use-module (utilities)
  #:use-module (gnu home services dotfiles)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop))

(define disable-mouse-acceleration
  (package
    (name "xorg-disable-mouse-acceleration")
    (version "0.0.0")
    (source (f "99-libinput-custom-config.conf"))
    (build-system (@ (guix build-system copy) copy-build-system))
    (arguments '(#:install-plan '(("." "share/X11/xorg.conf.d"))))
    (home-page "")
    (synopsis "Disable mouse acceleration")
    (description synopsis)
    (license (@ (guix licenses) cc0))))

(define bspwm-desktop-home-services
  (list
   (service home-dbus-service-type)
   (service
    home-dotfiles-service-type
    (home-dotfiles-configuration (directories (list "../dotfiles/bspwm"))))
   (simple-service
    'bspwm-files
    home-files-service-type
    `((".XCompose" ,(computed-file
                     "XCompose"
                     #~(with-output-to-file #$output
                         (lambda () (load  #$(f "xcompose.scm"))))))))
   (simple-service
    'bspwm-desktop-profile
    home-profile-service-type
    (using-nvidia
     (specifications->package-list
      disable-mouse-acceleration
      ;; Would probably need some kind of system service use nvidia modules.
      ;; Install this through Arch.
      "xinit" "xorg-server" "xf86-input-libinput" "xf86-video-nouveau" "xf86-video-nv" "xf86-video-fbdev"
      "sxhkd" "bspwm" "redshift" "xclip"
      "imagemagick" "dunst"
      "xset" "xsetroot" "xcompmgr" "xrdb" "setxkbmap" "xmodmap" "xinput" "xprop" "xwininfo"
      ;; screenshots
      "maim" "xdotool" "xdialog"
      ;; these are started automatically by dbus
      "xdg-desktop-portal")))))
