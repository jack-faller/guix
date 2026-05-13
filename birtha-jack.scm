(add-to-load-path (dirname (current-filename)))
(use-modules (guix gexp)
             (gnu services)
             (gnu home)
             (gnu packages base)
             (gnu home services)
             (gnu home services xdg)
             (utilities)
             (configuration sway-desktop)
             (nonguix multiarch-container)
             (nongnu packages game-client)
             (nongnu packages nvidia)
             (jack))

(replace-mesa
 (home-environment
  (services
   (cons*
    (service home-xdg-user-directories-service-type
             (home-xdg-user-directories-configuration
              (download "$HOME/Downloads")
              (documents "$HOME/Documents")
              (videos "$HOME/Videos")
              (pictures "$HOME/Pictures")
              (music "$HOME/Music")))
    (simple-service
     'java-wrappers
     home-files-service-type
     (map
      (lambda (version)
        (define v (number->string version))
        (define name (string-append "java-" v "-wrapper"))
        (list (string-append ".local/programs/" name)
              (shell-script
               name (string-append "guix shell openjdk@" v " -- java \"$@\""))))
      (iota 9 20)))
    (append sway-desktop-home-services jack-services)))
  (packages
   (append
    (specifications->package-list
     "easyeffects" "google-chrome-stable"
     "prismlauncher"
     "icedove-wayland"
     (nonguix-container->package
      (let ((container (steam-container-for nvda)))
        (nonguix-container
         (inherit container)
         (shared (cons* "/hdd" "/ssd" (ngc-shared container)))))))
    jack-packages))))
