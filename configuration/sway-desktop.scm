(define-module (configuration sway-desktop)
  #:export (sway-desktop-system-services sway-desktop-home-services sway-desktop-launch-command)

  #:use-module (utilities)
  #:use-module (packages wl-mirror)

  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages glib)
  #:use-module (rde home services wm))

(define sway-desktop-launch-command "dbus-run-session sway")

(define sway-desktop-system-services
  (list
   ; (simple-service 'sway-environment session-environment-service-type
   ; '(("CLUTTER_BACKEND" . "wayland")
   ; ("QT_QPA_PLATFORM" . "wayland")
   ; ("XDG_SESSION_TYPE" . "wayland")
   ; ("XDG_SESSION_DESKTOP" . "sway")
   ; ("XDG_CURRENT_DESKTOP" . "sway")))
   (service screen-locker-service-type
			(screen-locker-configuration
			 (name "swaylock")
			 (program (file-append swaylock-effects "/bin/swaylock"))
			 (using-pam? #t)
			 (using-setuid? #f)))))

(define sway-desktop-home-services
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (simple-service 'sway-desktop-config-files config-files-service-type
				   '((".config/sway" "sway/config")
					 (".config/waybar" "waybar")))
   (simple-service
	'sway-desktop-profile
	home-profile-service-type
	(specifications->package-list
	 "sway" "waybar" "swaylock-effects" "gammastep" "wl-clipboard" "xorg-server-xwayland"
	 "python-pywal" "imagemagick" "dunst" wl-mirror
	 ;; these are for binds
	 "brightnessctl" "pulseaudio"
	 ;; screenshots
	 "slurp" "grim" "xdg-user-dirs" "zenity"
	 ;; these are started automatically by dbus
	 "xdg-desktop-portal" "xdg-desktop-portal-wlr"))
   (service
	home-sway-service-type
	(home-sway-configuration
	 (config
	  `((exec --no-startup-id
			  ,(file-append dbus "/bin/dbus-update-activation-environment") --all)
		(bar swaybar_command ,(file-append waybar "/bin/waybar"))
		;; resolves files/programs/dmenu
		(set $menu dmenu)
		(set $menu_run rofi -show run)
		(set $volume ,(f "sway/scripts/vol"))
		(set $brightness ,(f "sway/scripts/light"))
		(set $mute ,(f "sway/scripts/mute"))
		(set $lock exec swaylock
			 --screenshots --clock --indicator
			 --indicator-radius 100
			 --indicator-thickness 12
			 --effect-blur 7x5
			 --effect-vignette 0.5:0.5)
		(seat seat0 xcursor_theme Quintom_Ink 12)
		(exec dunst -config ,(f "dunstrc") &)
		(exec gammastep -l 50.721680:-1.878530 &)
		(include "~/.config/sway/base-config")
		(exec ,(f "sway/startup-programs.sh"))))))))
