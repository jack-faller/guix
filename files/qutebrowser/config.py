from qutebrowser.keyinput import keyutils
config.load_autoconfig()
config.bind('<Escape>', 'mode-enter normal;; set statusbar.show in-mode', mode='command')
config.bind('<Return>', 'command-accept;; set statusbar.show in-mode', mode='command')
config.bind('eo', 'cmd-set-text :open google.com/search?q=')
config.bind('eO', 'cmd-set-text :open -t google.com/search?q=')
setshow = 'set statusbar.show always;; '
for i in ['o', 'O', 'T', ':', '/', 'eo', 'eO', 'go', 'gO']:
    s = config._keyconfig.get_command(keyutils.KeySequence.parse(i), 'normal')
    if not i.startswith(setshow):
        config.bind(i, setshow + s)
c.editor.command = ["emacsclient", "-nc", "--frame-parameters=((name . \"floating\") (width . 130) (height . 40))",
                    "+{line}:{column}", "{file}"]

config.bind("#", "tab-focus")
config.bind(";gv", "hint -r links spawn sh '-c' '~/.config/zsh/aliases/pla {hint-url}'")
config.bind(";s", "hint all run open https://yandex.com/images/search?rpt=imageview&url={hint-url}")
config.bind(";S", "hint all run open -t https://yandex.com/images/search?rpt=imageview&url={hint-url}")
config.bind(";v", "hint links spawn detach mpv {hint-url}")
config.bind("<Alt+b>", "spawn --userscript get.sh")
config.bind("<Alt+f>", "hint links spawn --detach mpv --force-window yes {hint-url}")
config.bind("<Alt+j>", "scroll down")
config.bind("<Alt+k>", "scroll up")
config.bind("<Space>", "tab-focus last")
config.bind("B", "spawn --userscript stow.sh -o")
config.bind("b", "spawn --userscript stow.sh")
config.bind("pu", "spawn --userscript unsee.sh {primary}")
config.bind("Pu", "spawn --userscript unsee.sh {primary} '-t'")
config.bind("pU", "spawn --userscript unsee.sh {clipboard}")
config.bind("PU", "spawn --userscript unsee.sh {clipboard} '-t'")
config.bind("et", "open -t youtube.com/feed/subscriptions")
config.bind("en", "open -t news.ycombinator.com")
config.bind("el", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_episode_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("eL", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_episode_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("j", "cmd-run-with-count 5 scroll down")
config.bind("k", "cmd-run-with-count 5 scroll up")

config.bind("*", "search {primary}")
config.bind("<Alt+Shift+*>", "search --reverse {primary}")

c.auto_save.session = True
c.colors.webpage.preferred_color_scheme = "dark"
c.fonts.web.family.fixed = "iosevka"
c.fonts.default_family = "iosevka"
c.fonts.default_size = "10pt"
c.hints.auto_follow = "always"
c.hints.chars = "abcdefghijklmnopqrstuvwxyz"
c.scrolling.bar = "always"
c.statusbar.show = "in-mode"
c.tabs.last_close = "close"
c.tabs.new_position.unrelated = "next"
c.tabs.show = "multiple"
c.editor.remove_file = False

c.spellcheck.languages = ["en-GB"]
