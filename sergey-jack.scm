(add-to-load-path (dirname (current-filename)))
(use-modules (guix gexp)
			 (gnu)
			 (gnu services)
			 (gnu services shepherd)
			 (gnu home)
			 (gnu home services)
			 (gnu home services xdg)
			 (gnu home services shells)
			 (gnu home services shepherd)
			 (nongnu packages game-client)
			 (nongnu packages fonts)
			 (gnu home-services emacs)

			 (packages miny)
			 (packages guix-dev)
			 (file-utils)
			 (configuration nix)
			 (configuration sway-desktop))
(use-package-modules
 emacs freedesktop gnupg wm python-xyz suckless shellutils bittorrent perl6 tor)

(define set-PATH "export PATH=\"$HOME/.local/programs:$PATH\"")

(home-environment
 (services
  ((lambda args (append args sway-desktop-home-services))
   (service home-xdg-user-directories-service-type
			(home-xdg-user-directories-configuration
			 (download "$HOME/dlds")
			 (documents "$HOME/docs")
			 (videos "$HOME/vids")
			 (pictures "$HOME/pics")
			 (music "$HOME/music")))
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
	 ;; TODO the arrows for magit sections don't work properly
	 (shepherd-service
	  (provision '(emacs-server))
	  (requirement '(ssh-agent))
	  (documentation "run emacs-server")
	  (start #~(make-forkexec-constructor
				(list #$(executable-shell-script
						 "emacs-daemon-script"
						 "source $XDG_RUNTIME_DIR/ssh-agent.env"
						 set-PATH
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
	  (provision '(fetch-icons))
	  (documentation "download the icons / fonts I want if they aren't already present")
	  (start
	   #~(make-system-constructor
		  #$(lines
			 "cd /tmp"
			 "mkdir -p ~/.local/share/fonts"
			 "mkdir -p ~/.icons"
			 "if [ ! -e ~/.local/share/fonts/materialdesignicons-webfont.ttf ]; then"
			 "  wget https://raw.githubusercontent.com/Templarian/MaterialDesign-Webfont/master/fonts/materialdesignicons-webfont.ttf "
			 "  mv materialdesignicons-webfont.ttf ~/.local/share/fonts/"
			 "fi"
			 "if [ ! -e ~/.icons/Quintom_Ink ]; then"
			 "  git clone https://gitlab.com/Burning_Cube/quintom-cursor-theme"
			 "  mv 'quintom-cursor-theme/Quintom_Ink Cursors/Quintom_Ink' ~/.icons"
			 "fi"
			 "if ! ls ~/.local/share/fonts/Crimson*; then "
			 "  git clone https://github.com/skosch/Crimson.git"
			 "  mv 'Crimson/Desktop Fonts/OTF/'* ~/.local/share/fonts/"
			 "fi")))
	  (one-shot? #t))
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
	config-files-service-type
	`((".config/tmux/tmux.conf" "tmux.conf")
	  (".config/kitty/kitty.conf" "kitty.conf")
	  (".config/miny/default.args" ,(plain-file "miny-default.args" "-d3"))
	  (".local/programs" "programs")
	  (".local/programs/raku" ,(file-append rakudo "/bin/perl6"))
	  (".local/programs/tor-qutebrowser"
	   ,(executable-shell-script
		 "tor-qutebrowser"
		 "herd start tor-client"
		 "qutebrowser --temp-basedir --set content.proxy socks://localhost:9050/ --config-py ~/.config/qutebrowser/config.py"
		 "herd stop tor-client"))
	  ;; duplicate to make passmenu work correctly
	  (".local/programs/dmenu-wl" "programs/dmenu")
	  (".config/git/config" "gitconfig")
	  (".config/qutebrowser/config.py" "qutebrowser/config.py")
	  (".local/share/qutebrowser/userscripts" "qutebrowser/userscripts")
	  (".config/wal/templates" "wal/templates")
	  (".gnupg/gpg-agent.conf"
	   ,(mixed-text-file
		 "gnupg-agent.conf"
		 "default-cache-ttl 600" "\n" "max-cache-ttl 7200" "\n"
		 "use-agent" "\n"
		 "pinentry-program " pinentry "/bin/pinentry" "\n"))
	  (".icons/default/index.theme"
	   ,(plain-file "cursor-theme-index" (lines "[Icon Theme]"
												"Name=Default"
												"Comment=Default Cursor Theme"
												"Inherits=Quintom_Ink")))
	  (".config/gtk-3.0/settings.ini" "gtk/3-settings.ini")
	  (".config/gtk-4.0/settings.ini" "gtk/4-settings.ini")))
   (service
	home-zsh-service-type
	(home-zsh-configuration
	 (zprofile
	  (list
	   (mixed-text-file
		"zsh-profile"
		python-pywal "/bin/wal -i \"$HOME\"/pics/wallpapers &> /dev/null" "\n"
		"brightnessctl set $(cat $XDG_CACHE_HOME/brightness_value)%" "\n"
		"export SYSTEM_DMENU='" dmenu "/bin/dmenu'" "\n"
		set-PATH "\n"
		"[ -z \"$DISPLAY\" ] && [ \"$XDG_VTNR\" = 1 ] && " sway-desktop-launch-command "\n")))
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
	 (early-init-el
	  `((load-file ,(f "emacs/early-init.el"))))
	 (init-el
	  `(;; Override later defvar of `using-guix' in init.el.
		(defvar using-guix t)
		(load-file ,(f "emacs/init.el"))
		(load-file ,(computed-file
					 "settings.el"
					 #~(system*
						(string-append #$emacs "/bin/emacs") "--batch" "--eval"
						(with-output-to-string
						  (lambda ()
							(write `(progn (require 'org)
										   (org-babel-tangle-file
											,#$(f "emacs/settings.org")
											,#$output
											"emacs-lisp"))))))))))))
   (service nix-packages-service-type
			'("discord" "teams" "tor-browser-bundle-bin"))))
 (packages
  (packages
   ;; basic
   "glibc" "ntfs-3g"
   "adwaita-icon-theme"
   "udiskie"
   "gnupg" "pinentry" ;; allows gnupg to prompt for password
   ;; editing
   "emacs" "emacs-all-the-icons" "hunspell" "hunspell-dict-en-gb"
   "perl" ;; needed for magit
   ;; for latex previews
   "texlive-scheme-basic" "texlive-ulem" "texlive-amsfonts"
   "cloc"
   "neovim"
   ;; zsh
   "zsh-syntax-highlighting" "zsh-autosuggestions"
   "password-store"
   ;; font
   "font-iosevka"
   "font-google-material-design-icons"
   "font-microsoft-impact" "font-ghostscript" "font-dejavu" "font-gnu-freefont"
   ;; cli utilities
   "eza" ;; ls alternative
   "git" "tmux" "rsync" "tree" "p7zip" "shellcheck" "glances"
   "dmenu"
   "zbar" ;; reads bar/qr codes for qute script
   "sbcl"
   "man-pages"
   "yt-dlp"
   "rakudo"
   "android-file-transfer"
   guix-dev
   ;; applications
   "kitty" "qutebrowser" "fontforge" (list transmission "gui")
   "xournalpp" "evince" "mpv" "feh" "ungoogled-chromium-wayland" "gimp"
   "xdg-utils" miny
   "obs" "obs-wlrobs"
   ;; NOTE: this should be steam-nvidia on nvidia systems
   "steam")))
