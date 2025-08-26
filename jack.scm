(define-module (jack)
  #:export (jack-services jack-packages)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services xdg)
  #:use-module (nongnu packages fonts)
  #:use-module (rde home services emacs)

  #:use-module (packages miny)
  #:use-module (packages discord)
  #:use-module (packages guix-dev)
  #:use-module (packages icons)
  #:use-module (packages pulseshitter)
  #:use-module (utilities))

(use-package-modules
 base emacs freedesktop gnupg wm python-xyz shellutils bittorrent perl6 tor
 libcanberra unicode)

(define jack-services
  (cons*
   (simple-service
    'my-channels
    home-channels-service-type
    (list
     (channel
      (name 'nonguix)
      (url "https://gitlab.com/nonguix/nonguix")
      (introduction
       (make-channel-introduction
        "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        (openpgp-fingerprint
         "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
     (channel
      (name 'guix-gaming-games)
      (url "https://gitlab.com/guix-gaming-channels/games.git")
      ;; Enable signature verification:
      (introduction
       (make-channel-introduction
        "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
        (openpgp-fingerprint
         "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
     (channel
      (name 'rde)
      (url "https://git.sr.ht/~abcdw/rde")
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))
   (service home-pipewire-service-type)
   (simple-service
    'my-env-vars
    home-environment-variables-service-type
    `(("PATH" . "$HOME/.local/programs:$PATH")
      ("DISCORD_ENABLE_UPDATES" . "true")
      ("UNICODE_DATA_TXT" .
       ,(file-append ucd "/share/ucd/UnicodeData.txt"))
      ,@(let ((askpass (program-file "askpass"
                                     #~(system* "zenity" "--password"))))
          ("SUDO_ASKPASS" . ,askpass)
          ("SSH_ASKPASS" . ,askpass))))
   (service
    home-xdg-mime-applications-service-type
    (home-xdg-mime-applications-configuration
     (default '((application/pdf . org.gnome.Evince.desktop)
                (application/x-torrent . transmission-gtk.desktop)
                (x-scheme-handler/magnet . transmission-gtk.desktop)
                (application/x-bittorrent . transmission-gtk.desktop)
                (image/jpeg . feh.desktop)
                (image/png . feh.desktop)
                (x-scheme-handler/http . org.qutebrowser.desktop)
                (x-scheme-handler/https . org.qutebrowser.desktop)))))
   (simple-service
    'my-daemons home-shepherd-service-type
    (list
     (shepherd-service
      (provision '(tor-client))
      (auto-start? #f)
      (respawn? #f)
      (requirement '())
      (documentation "run tor client")
      (start #~(make-forkexec-constructor
                (list #$(file-append tor-client "/bin/tor"))
                #:log-file (string-append (getenv "XDG_CACHE_HOME") "/tor.log")))
      (stop #~(make-kill-destructor)))
     (shepherd-service
      (provision '(ssh-agent))
      (documentation "run ssh-agent")
      (start #~(make-system-constructor
                "ssh-agent > $XDG_RUNTIME_DIR/ssh-agent.env"
                " 2> $XDG_CACHE_HOME/ssh-agent.log"))
      (stop #~(make-system-destructor "pkill ssh-agent")))
     (shepherd-service
      (provision '(emacs-server))
      (requirement '(ssh-agent))
      (documentation "run emacs-server")
      (start #~(make-forkexec-constructor
                (list #$(shell-script
                         "emacs-daemon-script"
                         "source $XDG_RUNTIME_DIR/ssh-agent.env"
                         "emacs --fg-daemon"))
                #:log-file (string-append (getenv "XDG_CACHE_HOME") "/emacs.log")))
      (stop #~(make-kill-destructor)))
     (shepherd-service
      (provision '(udiskie))
      (documentation "run udiskie")
      (start #~(make-forkexec-constructor
                (list #$(file-append udiskie "/bin/udiskie") "-N")
                #:log-file (string-append (getenv "XDG_CACHE_HOME") "/udiskie.log")))
      (stop #~(make-kill-destructor)))
     (shepherd-service
      (provision '(global-symlinks))
      (documentation "setup symlinks that I want")
      (start #~(make-system-constructor
                #$(lines
                   "mkdir -p ~/.local/share/Trash/files"
                   "ln -s ~/.local/share/Trash/files ~/trash"
                   "mkdir -p /media/jack"
                   "ln -s /media/jack ~/drives")))
      (one-shot? #t))))
   (service
    home-dotfiles-service-type
    (home-dotfiles-configuration (directories (list "dotfiles/jack"))))
   (service
    home-files-service-type
    `((".local/programs/raku" ,(file-append rakudo "/bin/perl6"))
      (".local/programs/clip" ,(c-script "clip" (f "clip.c")))
      (".local/programs/ardour"
       ,(program-file
         "ardour"
         #~(apply
            system* "pw-jack" #$(file-append (@ (gnu packages audio) ardour)
                                             "/bin/ardour8")
            (cdr (program-arguments)))))
      (".local/programs/percent" ,(rust-script "percent" (f "percent.rs")))
      ;; Need this here because dotfiles service removes .html extension.
      (".local/share/qutebrowser/userscripts/suppress.html" ,(f "suppress.html"))
      (".config/kitty/bell.oga"
       ,(file-append sound-theme-freedesktop "/share/sounds/freedesktop/stereo/bell.oga"))
      (".gnupg/gpg-agent.conf"
       ,(mixed-text-file
         "gnupg-agent.conf"
         "default-cache-ttl 600" "\n" "max-cache-ttl 7200" "\n"
         "pinentry-program " pinentry "/bin/pinentry" "\n"))
      (".local/share/icons/default/index.theme"
       ,(plain-file "cursor-theme-index" (lines "[Icon Theme]"
                                                "Name=Default"
                                                "Comment=Default Cursor Theme"
                                                "Inherits=Quintom_Ink")))
      (".local/share/icons/Quintom_Ink"
       ,(file-append quintom-cursor-theme "/share/icons/Quintom_Ink"))))
   (service
    home-zsh-service-type
    (home-zsh-configuration
     (zprofile
      (list
       (f "zprofile.sh")))
     (zshrc
      (list
       (f "zshrc.sh")
       (mixed-text-file
        "source-zsh-extensions"
        "source " zsh-autosuggestions "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
        "\n"
        ;; this must be the last item in zshrc for some reason
        "source " zsh-syntax-highlighting "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        "\n")))))
   (service
    home-emacs-service-type
    (home-emacs-configuration
     (emacs-servers '())
     (early-init-el
      `((load-file ,(f "emacs/early-init.el"))))
     (init-el
      `( ;; Override later defvar of `using-guix' in init.el.
        (defvar using-guix t)
        (setf insert-directory-program ,(file-append coreutils "/bin/ls"))
        (load-file ,(f "emacs/init.el"))
        (load-file ,(computed-file
                     "settings.el"
                     (with-imported-modules '((guix build utils))
                       #~(begin
                           (use-modules (guix build utils))
                           (invoke
                            (string-append #$emacs "/bin/emacs")
                            "--no-init-file" "--batch" "--eval"
                            (with-output-to-string
                              (lambda ()
                                (write `(progn (require 'org)
                                               (org-babel-tangle-file
                                                ,#$(f "emacs/settings.org")
                                                ,#$output
                                                "emacs-lisp"))))))))))))
     ;; Don't override Emacs from package list.
     (emacs hello)))
   (simple-service 'my-fonts
                   home-fontconfig-service-type
                   (list
                    '(alias
                      (family "serif")
                      (prefer (family "Noto Serif")
                              (family "Noto Serif CJK SC")
                              (family "Noto Serif CJK JP")
                              (family "Noto Serif CJK TC")
                              (family "Noto Color Emoji")))
                    '(alias
                      (family "sans-serif")
                      (prefer (family "Noto Sans")
                              (family "Noto Sans CJK SC")
                              (family "Noto Sans CJK JP")
                              (family "Noto Sans CJK TC")
                              (family "Noto Color Emoji")))
                    '(alias
                      (family "monospace")
                      (prefer (family "Iosevka")
                              (family "Noto Sans Mono")
                              (family "Noto Sans Mono CJK SC")
                              (family "Noto Sans Mono CJK JP")
                              (family "Noto Sans Mono CJK TC")))
                    '(alias
                      (family "emoji")
                      (prefer (family "Noto Color Emoji")))))
   %base-home-services))
(define jack-packages
  (specifications->package-list
   ;; basic
   "ntfs-3g"
   "adwaita-icon-theme"
   "udiskie"
   "gnupg" "pinentry" ;; allows gnupg to prompt for password
   ;; editing
   "emacs" "emacs-all-the-icons" "hunspell" "hunspell-dict-en-gb"
   "perl"        ;; needed for magit
   "gcc-toolchain" ;; needed to compile treesitter grammars
   ;; for latex previews
   "texlive-scheme-basic" "texlive-collection-fontsrecommended"
   "texlive-dvipng-bin" "texlive-ulem" "texlive-amsfonts"
   ;; zsh
   "zsh-syntax-highlighting" "zsh-autosuggestions"
   "password-store"
   ;; font
   "font-iosevka"
   font-google-material-design-icons-desktop font-cochineal
   "font-microsoft-impact" "font-ghostscript" "font-dejavu" "font-gnu-freefont" "font-google-noto" "font-google-noto-emoji" "font-google-noto-sans-cjk" "font-google-noto-serif-cjk"
   quintom-cursor-theme
   ;; cli utilities
   "zenity"
   "cloc"
   "pipewire" ;; gives pw-play
   "eza"      ;; ls alternative
   "socat" "curl"
   "hyperfine"
   "cups"
   "python-pywal"
   "xdg-user-dirs"
   "git" "tmux" "rsync" "tree" "p7zip" "shellcheck" "glances"
   "rofi"
   "zbar" ;; reads bar/qr codes for qute script
   "sbcl"
   "man-pages"
   "yt-dlp" "rakudo" "android-file-transfer" "xdg-utils"
   guix-dev
   ;; applications
   "cmst"
   discord pulseshitter "qutebrowser"
   "kitty" "gucharmap" "transmission:gui"
   "xournalpp" "evince" "mpv" "feh" "gimp"
   "kdenlive" "obs" miny
   "rhythmbox" "gst-libav" "gst-plugins-bad" "gst-plugins-ugly"
   "libreoffice"
   ))
