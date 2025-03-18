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
  #:use-module (gnu packages glib)
  #:use-module (rde home services wm))

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
                     ;; ("XDG_SESSION_DESKTOP" . "sway")
                     ;; ("XDG_CURRENT_DESKTOP" . "sway")
                     ))
   (simple-service
    'sway-desktop-profile
    home-profile-service-type
    (specifications->package-list
     "sway" "xorg-server-xwayland"
     "waybar" "swaylock-effects" "gammastep"
     "imagemagick" "dunst" "wl-mirror"
     "wtype" "wl-clipboard-x11" "wl-clipboard"
     ;; Make QT work, but this might not work for QT 5.
     "qtwayland"
     ;; these are for binds
     "brightnessctl" "pulseaudio"
     ;; screenshots
     "slurp" "grim" "zenity"
     ;; these are started automatically by dbus
     "xdg-desktop-portal" "xdg-desktop-portal-wlr"))
   (service
    home-sway-service-type
    (home-sway-configuration
     (config
      `((exec --no-startup-id
              ,(file-append dbus "/bin/dbus-update-activation-environment") --all)
        (bar swaybar_command ,(file-append waybar "/bin/waybar"))
        (set $menu dmenu)
        (set $menu_run rofi -show run)
        (set $volume "~/.config/sway/scripts/vol")
        (set $brightness "~/.config/sway/scripts/light")
        (set $mute "~/.config/sway/scripts/mute")
        (set $lock exec swaylock
             --screenshots --clock --indicator
             --indicator-radius 100
             --indicator-thickness 12
             --effect-blur 7x5
             --effect-vignette 0.5:0.5)
        (seat seat0 xcursor_theme Quintom_Ink 12)
        (exec dunst &)
        (exec gammastep -l 50.721680:-1.878530 &)
        (include "~/.config/sway/configs/base-config")
        (exec "~/.config/sway/startup-programs.sh")))))))
