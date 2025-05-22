from qutebrowser.keyinput import keyutils
config.load_autoconfig()
config.bind('<Escape>', 'mode-enter normal;; set statusbar.show in-mode', mode='command')
config.bind('<Return>', 'command-accept;; set statusbar.show in-mode', mode='command')
config.bind('eo', 'cmd-set-text :open google.com/search?q=')
config.bind('eO', 'cmd-set-text :open -t google.com/search?q=')
c.editor.command = ["e", "-c", "+{line}:{column}", "{file}"]

def script(name):
    return "~/.local/share/qutebrowser/userscripts/" + name

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
config.bind(";i", "hint images yank")
config.bind(";I", "hint images yank --rapid")
config.bind("es", "tab-move end;; spawn --userscript " + script("shuffle.scm"))
config.bind("er", "tab-focus -1;; spawn --userscript " + script("random.scm"))
config.bind("B", "spawn --userscript stow.sh -o")
config.bind("b", "spawn --userscript stow.sh")
config.bind("gs", "spawn --userscript suppress")
config.bind("pu", "spawn --userscript unsee.sh {primary}")
config.bind("Pu", "spawn --userscript unsee.sh {primary} '-t'")
config.bind("pU", "spawn --userscript unsee.sh {clipboard}")
config.bind("PU", "spawn --userscript unsee.sh {clipboard} '-t'")
config.bind("et", "open -t youtube.com/feed/subscriptions")
config.bind("en", "open -t news.ycombinator.com")
config.bind("el", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_episode_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("eL", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_episode_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("el", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_release_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("eL", "open -t https://www.livechart.me/users/xeere/library?layout=regular&page=1&sort=next_release_countdown&statuses%5B%5D=rewatching&statuses%5B%5D=watching&statuses%5B%5D=planning&statuses%5B%5D=considering&titles=romaji")
config.bind("j", "cmd-run-with-count 5 scroll down")
config.bind("k", "cmd-run-with-count 5 scroll up")

config.bind("*", "search {primary}")
config.bind("<Alt+Shift+*>", "search --reverse {primary}")

config.set("content.javascript.clipboard", "access", "www.nytimes.com")

c.auto_save.session = True
c.colors.webpage.preferred_color_scheme = "dark"
c.downloads.open_dispatcher = "xdg-open"
c.downloads.remove_finished = 0
c.editor.remove_file = False
c.fonts.default_family = "iosevka"
c.fonts.default_size = "10pt"
c.fonts.web.family.fixed = "iosevka"
c.hints.auto_follow = "always"
c.hints.chars = "abcdefghijklmnopqrstuvwxyz"
c.scrolling.smooth = True
c.statusbar.show = "in-mode"
c.tabs.last_close = "close"
c.tabs.new_position.unrelated = "next"
c.tabs.show = "multiple"

c.spellcheck.languages = ["en-GB"]
