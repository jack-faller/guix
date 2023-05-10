(use-modules (ice-9 textual-ports)
			 (ice-9 ftw)
			 (ice-9 popen)
			 (ice-9 rdelim)
			 (guix gexp)
			 (gnu)
			 (gnu services)
			 (gnu services shepherd)
			 (gnu home)
			 (gnu home services)
			 (gnu home services xdg)
			 (gnu home services shells)
			 (gnu home services shepherd)
			 (nongnu packages steam-client)
			 (rde home services wm)
			 (gnu home-services emacs))
(use-package-modules
 shellutils vim emacs emacs-xyz rust-apps rsync web bash admin version-control
 password-utils wm tmux ssh terminals fonts glib imagemagick xdisorg
 fontutils ghostscript web-browsers bittorrent suckless linux gnome aidc
 gnuzilla freedesktop package-management pulseaudio gnupg admin compression
 haskell-apps pdf video xdisorg gcc libreoffice aspell tex python-xyz xorg code
 lisp image man chromium gimp base hunspell freedesktop)

(define my-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "en_GB")
   #:name "glibc-english-utf8-locales"))

(define config-directory (dirname (current-filename)))
(define (fname . x) (apply string-append config-directory "/" x))
(define (f . x) (local-file (apply fname x)))
(define (not-dot file) (not (or (string= file ".") (string= file ".."))))
(define (lines . lines) (string-join lines "\n" 'suffix))

(define (executable-file file)
  (chmod file #o777)
  (local-file file #:recursive? #t))
(define (fexec . x) (executable-file (apply fname x)))
(define (executable-shell-script name . lines-list)
  (define script (apply lines (cons "#!/bin/sh" lines-list)))
  (computed-file name #~(begin
						  (use-modules (ice-9 ports))
						  (call-with-output-file #$output
							(λ (file) (display #$script file)))
						  (chmod #$output #o555))))

(define nix-profile-service-type
  (let* ((switch-to-generstion
		  (λ (generation)
			#~(system* "nix-env" "--switch-generation" #$generation)))
		 (zsh-nix-zprofile
		  (plain-file
		   "nix-source-command"
		   "source /run/current-system/profile/etc/profile.d/nix.sh"))
		 (zsh-extension
		  (const (home-zsh-extension (zprofile (list zsh-nix-zprofile))))))
	(service-type
	 (name 'nix-packages)
	 (extensions
	  (list
	   (service-extension home-activation-service-type switch-to-generstion)
	   (service-extension home-zsh-service-type zsh-extension)))
	 (description
	  "A home service that switches to a generation of the user's nix profile.
Its value is a string containing the number of the generation to switch to."))))
;; run a script to install the packages
(define (generate-nix-packages-service . packages)
  (define (assert-nix-success return-code)
	(unless (= 0 return-code)
	   (error "failed to run nix command")))
  (define package-file
	(string-append (getenv "HOME") "/.cache/installed-nix-packages"))
  (assert-nix-success
   (system
	(lines
	 "if [ \"$(nix-channel --list)\" = \"\" ]; then"
	 "  nix-channel --add https://nixos.org/channels/nixpkgs-unstable"
	 "  nix-channel --update"
	 "  nix-env -iA nixpkgs.nix nixpkgs.cacert"
	 (string-append "echo '()' > " package-file)
	 "fi")))
  (unless (equal? packages (with-input-from-file package-file read))
	(with-output-to-file package-file (λ () (write packages) (newline)))
	(let* ((cmd (cons*
				 "NIXPKGS_ALLOW_UNFREE=1" "nix-env" "--remove-all" "--install"
				 packages))
		   (cmd (if (null? packages) '("nix-env" "--uninstall" "'.*'") cmd))
		   (cmd (string-join cmd " ")))
	  (assert-nix-success (system cmd))))
  (service
   nix-profile-service-type
   (let* ((pipe
		   (open-input-pipe
			"nix-env --list-generations | grep current | grep --only-matching '[0-9]*'"))
		  (output (read-line pipe)))
	 (assert-nix-success (close-pipe pipe))
	 output)))

(home-environment
 (services
  (list
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
				(application/x-bittorrent . transmission-gtk.desktop)))))
   (simple-service
	'my-daemons home-shepherd-service-type
	(list
	 (shepherd-service
	  (provision '(ssh-agent))
	  (documentation "run ssh-agent")
	  (start #~(make-system-constructor
				"ssh-agent > $XDG_RUNTIME_DIR/ssh-agent.env"
				" 2> $XDG_LOG_HOME/ssh-agent.log"))
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
						 "emacs --fg-daemon"))
				#:log-file (string-append (getenv "XDG_LOG_HOME") "/emacs.log")))
	  (stop #~(make-kill-destructor)))
	 (shepherd-service
	  (provision '(udiskie))
	  (documentation "run udiskie")
	  (start #~(make-forkexec-constructor
				(list #$(file-append udiskie "/bin/udiskie") "-N")
				#:log-file (string-append (getenv "XDG_LOG_HOME") "/udiskie.log")))
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
   (simple-service
	'my-config-manager home-files-service-type
	(let* ((map2 (λ (f1 f2 lst) (map (λ (a) (list (f1 a) (f2 a))) lst)))
		   (dir-files (λ (dir) (scandir dir not-dot)))
		   (pre
			(λ pre (λ (name) (string-append (apply string-append pre) name))))
		   (from-dir
			(λ (dir files)
			  (map2 (pre ".config/" dir "/") (λ (x) (f "files/" dir "/" x))
					files)))
		   (whole-dir
			(λ (dir target)
			  (map2 (pre target "/")
					(λ (file) (f "files/" dir "/" file))
					(dir-files (fname "files/" dir))))))
	  `((".config/tmux/tmux.conf" ,(f "files/tmux.conf"))
		,@(from-dir "sway" '("base-config" "binds" "input" "output"))
		,@(whole-dir "waybar" ".config/waybar")
		(".config/kitty/kitty.conf" ,(f "files/kitty.conf"))
		,@(map2 (pre ".local/programs/")
				(λ (file) (fexec "files/programs/" file))
				(dir-files (fname "files/programs/")))
		;; duplicate to make passmenu work correctly
		(".local/programs/dmenu-wl" ,(fexec "files/programs/dmenu"))
		(".local/programs/em"
		 ,(program-file "em-script" #~(apply system* "emacsclient" "-nw"
											 (cdr (program-arguments)))))
		(".local/programs/dev"
		 ,(program-file
		   "dev-script"
		   #~(begin
			   (use-modules (srfi srfi-1))
			   (apply system*
					  "guix" "shell"
					  (fold
					   (lambda (arg acc)
						 (let ((file (string-append "/config/dev/" arg ".scm")))
						   (cond
							((equal? arg "-r") (cons "--rebuild-cache" acc))
							((file-exists? file) (cons* "-f" file acc))
							(else (cons arg acc)))))
					   (let loop ((dir (getcwd)))
						 (let ((guixscm (string-append dir "/guix.scm")))
						   (cond
							((file-exists? guixscm) `("-D" "-f" ,guixscm))
							((equal? dir "/") '())
							(else (loop (dirname dir))))))
					   (cdr (program-arguments)))))))
		(".config/git/config" ,(f "files/gitconfig"))
		,@(whole-dir "qutebrowser" ".config/qutebrowser")
		,@(whole-dir "wal/templates" ".config/wal/templates")
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
		(".config/gtk-3.0/settings.ini" ,(f "files/gtk/3-settings.ini"))
		(".config/gtk-4.0/settings.ini" ,(f "files/gtk/4-settings.ini")))))
   (service
	home-sway-service-type
	(home-sway-configuration
	 (config
	  `((exec ,(file-append dbus "/bin/dbus-update-activation-environment")
			  WAYLAND_DISPLAY XDG_CURRENT_DESKTOP)
		(bar swaybar_command ,(file-append waybar "/bin/waybar"))
		;; resolves files/programs/dmenu
		(set $menu dmenu)
		(set $volume ,(fexec "files/sway/scripts/vol"))
		(set $brightness ,(fexec "files/sway/scripts/light"))
		(set $mute ,(fexec "files/sway/scripts/mute"))
		(set $lock exec swaylock
			 --screenshots --clock --indicator
			 --indicator-radius 100
			 --indicator-thickness 12
			 --effect-blur 7x5
			 --effect-vignette 0.5:0.5
			 --fade-in 0.2)
		(seat seat0 xcursor_theme Quintom_Ink 12)
		(exec fnott --config=.cache/wal/fnott.ini &)
		(include "~/.config/sway/base-config")
		(exec ,(fexec "files/sway/startup-programs.sh"))))))
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
		"export PATH=\"$HOME/.local/programs:$PATH\"" "\n")))
	 (zshrc
	  (list
	   (mixed-text-file "assign-vim-sys" "VIM_SYS='" vim "/bin/vim'" "\n")
	   (f "files/zshrc.sh")
	   (mixed-text-file
		"source-zsh-extensions"
		"source " zsh-autosuggestions "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
		"\n"
		;; this must be the last item in zshrc for some reason
		"source " zsh-syntax-highlighting "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
		"\n"
		"pgrep sway &> /dev/null || " sway "/bin/sway" "\n")))))
   (service
	home-emacs-service-type
	(home-emacs-configuration
	 (early-init-el
	  `((load-file ,(f "files/emacs/early-init.el"))))
	 (init-el
	  `((require 'emacsql-sqlite)
		;; handle special hunspell dictionary name
		;; later defvar will be overridden by this
		(defvar dict "en_GB-ise")
		(load-file ,(f "files/emacs/init.el"))
		(let ((settings (concat user-emacs-directory "settings.org")))
		  (org-babel-tangle-file ,(f "files/emacs/settings.org") settings)
		  (load-file settings))))))
   (generate-nix-packages-service
	"discord" "teams" "tor-browser-bundle-bin")))
 (packages
  (list
   ;; basic
   glibc my-glibc-locales ntfs-3g
   adwaita-icon-theme
   udiskie
   pulseaudio
   gnupg pinentry ;; allows gnupg to prompt for password
   ;; wm
   sway waybar gammastep wl-clipboard fnott xorg-server-xwayland
   python-pywal imagemagick
   brightnessctl
   kitty
   slurp grim xdg-user-dirs zenity ;; screenshots
   ;; editing
   emacs emacs-all-the-icons emacs-emacsql-sqlite3 hunspell hunspell-dict-en-gb
   ;; for latex previews
   texlive-base texlive-ulem texlive-amsfonts
   cloc
   vim
   ;; zsh
   zsh-syntax-highlighting zsh-autosuggestions
   password-store
   ;; font
   font-iosevka
   font-google-material-design-icons
   gs-fonts font-dejavu font-gnu-freefont
   ;; cli utilities
   exa ;; ls alternative
   git tmux rsync tree p7zip shellcheck glances
   dmenu
   zbar ;; reads bar/qr codes for qute script
   sbcl
   man-pages
   ;; applications
   qutebrowser fontforge (list transmission "gui") icedove xournalpp evince mpv
   ungoogled-chromium gimp
   xdg-utils
   ;; NOTE: this should be steam nvidia on nvidia systems
   steam)))
