(define-module (packages discord)
  #:export (discord)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system glib-or-gtk)

  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)

  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses)
                #:prefix license:))

(define disable-breaking-updates
  (with-extensions
   (list (@ (gnu packages guile) guile-json-4))
   (program-file
    "disable-breaking-updates"
    ;; Based on Python script of the same name from Nix.
    #~(begin
        (use-modules (json))
        (let* ((config-home (or (getenv "XDG_CONFIG_HOME")
                                (string-append (getenv "HOME") "/.config")))
               (settings-path (string-append config-home
                                             "/discord/settings.json"))
               (settings-tmp (string-append settings-path ".tmp"))
               (settings
                (if (file-exists? settings-path)
                    (with-exception-handler
                        (lambda (_)
                          (display "Settings invalid")
                          (newline)
                          (exit 0))
                      (lambda () (with-input-from-file settings-path json->scm))
                      #:unwind? #t
                      #:unwind-for-type 'json-invalid)
                    (list))))
          (if (assoc-ref settings "SKIP_HOST_UPDATE")
              (begin (display "Updates already disabled")
                     (newline))
              (begin
                (with-output-to-file settings-tmp
                  (lambda ()
                    (scm->json (assoc-set! settings "SKIP_HOST_UPDATE" #t))))
                (rename-file settings-tmp settings-path)
                (display "Disabled updates")
                (newline))))))))

(define discord-install
  (with-imported-modules
   '((guix build utils))
   #~(lambda* (#:key outputs inputs #:allow-other-keys)
       (use-modules (guix build utils)
                    (srfi srfi-26)
                    (sxml simple))
       (let* ((line (lambda args
                      (display (apply string-append args)) (newline)))
              (output (assoc-ref outputs "out"))
              (fonts (list "font-google-noto" "font-dejavu" "font-awesome"
                           "font-google-noto-emoji" "font-google-noto-sans-cjk"
                           "font-google-noto-serif-cjk"))
              (libs (cons (string-append (assoc-ref inputs "nss") "/lib/nss")
                          (map (lambda (i) (string-append (cdr i) "/lib"))
                               inputs)))
              (bins (map (lambda (i) (string-append (cdr i) "/bin"))
                         inputs))
              (libs (filter file-exists? libs))
              (bins (filter file-exists? bins)))
         (mkdir-p (string-append output "/opt/discord"))
         (copy-recursively "." (string-append output "/opt/discord"))
         (mkdir-p (string-append output "/bin"))
         (mkdir-p (string-append output "/share/pixmaps"))
         (mkdir-p (string-append output "/share/applications"))
         (mkdir-p (string-append output "/share/icons/hicolor/256x256/apps"))
         (mkdir-p (string-append output "/etc"))
         (with-output-to-file (string-append output "/etc/fonts.conf")
           (lambda _
             (line "<?xml version='1.0'?>")
             (line "<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>")
             (sxml->xml `(fontconfig ,@(map (lambda (f)
                                              `(dir ,(assoc-ref inputs f)))
                                            fonts)))))
         (with-output-to-file (string-append output "/bin/discord")
           (lambda _
             (line "#!/bin/sh")
             (line #$disable-breaking-updates)
             (line "cd " output "/opt/discord")
             (line "./Discord"
                   ;; Always use Ozone on Wayland, not sure if this is a good idea.
                   " ${WAYLAND_DISPLAY:+--enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WebRTCPipeWireCapturer}"
                   " \"$@\"")))
         (chmod (string-append output "/bin/discord") #o755)
         (wrap-program
          (string-append output "/bin/discord")
          `("LD_LIBRARY_PATH" = (,(string-append output "/opt/discord") ,@libs))
          `("FONTCONFIG_FILE" = (,(string-append output "/etc/fonts.conf")))
          `("PATH" prefix ,bins))
         (for-each
          (lambda (f)
            (chmod (string-append output "/opt/discord/" f) #o755)
            (invoke #+(file-append patchelf "/bin/patchelf")
                    "--set-interpreter"
                    (string-append #$glibc "/lib/ld-linux-x86-64.so.2")
                    (string-append output "/opt/discord/" f)))
          (list "Discord" "chrome_crashpad_handler" "chrome-sandbox"))
         (chmod (string-append output "/opt/discord/postinst.sh") #o755)
         (link (string-append output "/opt/discord/discord.png")
               (string-append output "/share/pixmaps/discord.png"))
         (link (string-append output "/opt/discord/discord.png")
               (string-append output "/share/icons/hicolor/256x256/apps/discord.png"))
		 (link (string-append output "/opt/discord/discord.desktop")
			   (string-append output "/share/applications/discord.desktop"))
		 (substitute*
		  (string-append output "/share/applications/discord.desktop")
		  (("Exec=.*$") (string-append "Exec=" output "/bin/discord\n"))
		  (("Path=.*$") (string-append "Path=" output "/opt/discord\n")))
         #t))))

(define discord
  (package
   (name "discord")
   (version "0.0.51")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://cdn.discordapp.com/apps/linux/" version
                         "/discord-" version ".tar.gz"))
     (sha256
      (base32 "12d5hghnn6a30szsdbay5rx5j31da8y51zxmxg4dcpc9m9wwpk63"))))
   ;; Use this build system to set XDG variables.
   (build-system glib-or-gtk-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'bootstrap)
                      (delete 'configure)
                      (delete 'build)
                      (delete 'check)
                      (replace 'install #$discord-install))))
   (inputs (list alsa-lib
                 at-spi2-core
                 cairo
                 cups
                 dbus
                 eudev
                 expat
                 fontconfig
                 freetype
                 ffmpeg
                 (list gcc "lib")
                 gdk-pixbuf
                 glib
                 gtk+
                 libappindicator
                 libcxx
                 libdbusmenu
                 libdrm
                 libglvnd
                 libnotify
                 libx11
                 libxcb
                 libxcomposite
                 libxcursor
                 libxdamage
                 libxext
                 libxfixes
                 libxi
                 libxrandr
                 libxrender
                 libxscrnsaver
                 libxshmfence
                 libxtst
                 mesa
                 nspr
                 nss
                 pango
                 util-linux
                 wayland

                 xdg-utils
                 pulseaudio

                 glibc

                 font-awesome
                 font-dejavu
                 font-google-noto
                 font-google-noto-emoji
                 font-google-noto-sans-cjk
                 font-google-noto-serif-cjk

                 clang-runtime
                 pipewire
                 libsm
                 node
                 unzip
                 gzip
                 wget))
   (synopsis "Discord chat client")
   (description "All-in-one cross-platform voice and text chat for gamers")
   (license (license:nonfree "https://discord.com/terms"))
   (home-page "https://discordapp.com")))
discord
