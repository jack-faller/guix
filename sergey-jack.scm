(add-to-load-path (dirname (current-filename)))
(use-modules (guix gexp)
             (gnu home)
             (gnu home services)
             (gnu home services xdg)
             (utilities)
             (configuration sway-desktop)
             (jack))

(home-environment
 (services
  (cons*
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (download "$HOME/dlds")
             (documents "$HOME/docs")
             (videos "$HOME/vids")
             (pictures "$HOME/pics")
             (music "$HOME/music")))
   (append sway-desktop-home-services jack-services)))
 (packages
  (append
   (apply
    specifications->package-list
    (map wayland-package-substitute
         (cons* "steam" "ungoogled-chromium-wayland" jack-packages))))))
