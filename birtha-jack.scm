(add-to-load-path (dirname (current-filename)))
(use-modules (guix gexp)
             (gnu services)
             (gnu home)
             (gnu packages base)
             (gnu home services)
             (gnu home services xdg)
             (utilities)
             (configuration bspwm-desktop)
             (nonguix multiarch-container)
             (nongnu packages game-client)
             (nongnu packages nvidia)
             (jack))

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
   (append bspwm-desktop-home-services jack-services)))
 (packages
  (using-nvidia
   (append
    (specifications->package-list
     (list "lsp-plugins" "lv2") "easyeffects" "ungoogled-chromium"
     "prismlauncher"
     "openjdk@21"
     (nonguix-container->package
      (let ((container (steam-container-for nvda)))
        (nonguix-container
         (inherit container)
         (shared (cons* "/hdd" "/ssd" (ngc-shared container)))))))
    jack-packages))))
