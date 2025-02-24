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
     (nonguix-container->package
      (nonguix-container
       (inherit steam-nvidia-container)
       (shared (cons* "/hdd" "/ssd" (ngc-shared steam-nvidia-container))))))
    jack-packages))))
