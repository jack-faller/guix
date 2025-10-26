(define-module (configuration sway-desktop)
  #:export (sway-desktop-system-services sway-desktop-home-services)

  #:use-module (utilities)
  #:use-module (gnu home services dotfiles)

  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages glib))

(define sway-desktop-system-services
  (list
   (service screen-locker-service-type
                        (screen-locker-configuration
                         (name "swaylock")
                         (program (file-append swaylock-effects "/bin/swaylock"))
                         (using-pam? #t)
                         (using-setuid? #f)))))

(define sway-desktop-home-services
  (list
   (service home-dbus-service-type)
   (service
    home-dotfiles-service-type
    (home-dotfiles-configuration (directories (list "../dotfiles/sway"))))
   (simple-service 'sway-environment-variables
                   home-environment-variables-service-type
                   '(("QT_QPA_PLATFORM" . "wayland;xcb")
                     ;; Not sure if these should be set.
                     ("XDG_SESSION_DESKTOP" . "sway")
                     ("XDG_CURRENT_DESKTOP" . "sway")
                     ))
   (simple-service
    'sway-desktop-profile
    home-profile-service-type
    (specifications->package-list
     "sway" "xorg-server-xwayland"
     "rofi"
     "waybar" "swaylock-effects" "gammastep"
     "imagemagick" "dunst" "wl-mirror"
     "wtype" "wl-clipboard-x11" "wl-clipboard"
     "wdisplays"
     ;; Make QT work, but this might not work for QT 5.
     "qtwayland"
     ;; these are for binds
     "brightnessctl" "pulseaudio"
     ;; screenshots
     "slurp" "grim"
     ;; these are started automatically by dbus
     "xdg-desktop-portal" "xdg-desktop-portal-gtk" "xdg-desktop-portal-wlr"))))
