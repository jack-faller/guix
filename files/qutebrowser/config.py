from qutebrowser.keyinput import keyutils
config.load_autoconfig()
config.bind('<Escape>', 'mode-enter normal;; set statusbar.show in-mode', mode='command')
config.bind('<Return>', 'command-accept;; set statusbar.show in-mode', mode='command')
config.bind('eo', 'set-cmd-text :open google.com/search?q=')
config.bind('eO', 'set-cmd-text :open -t google.com/search?q=')
for i in ['o', 'O', 'T', ':', '/', 'eo', 'eO', 'go', 'gO']:
    config.bind(i, 'set statusbar.show always;; '
                + config._keyconfig.get_command(keyutils.KeySequence.parse(i), 'normal'))
c.editor.command = ["emacswindow", "{file}", "--eval", "(text-mode)",
                    "--position", "{line}", "{column}"]

config.bind("#", "tab-focus")
config.bind(";gv", "hint -r links spawn sh '-c' '~/.config/zsh/aliases/pla {hint-url}'")
config.bind(";s", "hint all run open https://yandex.com/images/search?rpt=imageview&url={hint-url}")
config.bind(";S", "hint all run open -t https://yandex.com/images/search?rpt=imageview&url={hint-url}")
config.bind(";v", "hint links spawn detach mpv {hint-url}")
config.bind("<Alt+b>", "spawn --userscript ~/.config/qutebrowser/get.sh")
config.bind("<Alt+f>", "hint links spawn --detach mpv --force-window yes {hint-url}")
config.bind("<Alt+j>", "scroll down")
config.bind("<Alt+k>", "scroll up")
config.bind("<Space>", "tab-focus last")
config.bind("B", "spawn --userscript ~/.config/qutebrowser/stow.sh -o")
config.bind("b", "spawn --userscript ~/.config/qutebrowser/stow.sh")
config.bind("et", "open -t youtube.com/feed/subscriptions")
config.bind("el", "open -t https://www.livechart.me/users/xeere/library?completed=false&considering=true&layout=grid")
config.bind("eL", "open -t https://www.livechart.me/users/xeere/library?completed=false&considering=false&layout=grid")
config.bind("j", "run-with-count 5 scroll down")
config.bind("k", "run-with-count 5 scroll up")

c.auto_save.session = True
c.colors.webpage.preferred_color_scheme = "dark"
c.fonts.web.family.fixed = "iosevka"
c.fonts.default_family = "iosevka"
c.hints.auto_follow = "always"
c.hints.chars = "abcdefghijklmnopqrstuvwxyz"
c.scrolling.bar = "always"
c.statusbar.show = "in-mode"
c.tabs.last_close = "close"
c.tabs.new_position.unrelated = "next"

c.spellcheck.languages = ["en-GB"]
c.content.headers.user_agent = 'Linux / Firefox 82'
c.content.autoplay = False
