HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep
bindkey -v

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu
zmodload zsh/complist
compinit
_comp_options+=(globdots)       # Include hidden files.
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*' #moar fuzz

bindkey '^[[Z' reverse-menu-complete
bindkey '^[[Z' reverse-menu-complete

export EDITOR="e"
export VISUAL="$EDITOR"
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

if [ -n "$GUIX_ENVIRONMENT" ]; then
	export PS1="%K{red}%F{black}%n@%m%f%k %F{blue}[guix shell] %~%f %#"
else
	export PS1="%K{red}%F{black}%n@%m%f%k %F{blue}%~%f %#"
fi

alias la="ls -a"
alias ll="ls -la"
alias se="sudoedit"
alias off="sudo halt"
alias glances="glances -1"
alias please="sudo"
alias tm="mv --verbose --backup --target-directory ~/trash"
alias pass="EDITOR='vim' pass"

cd-then () {
  cd $2 && eval $1
}

alias cd="cd-then 'ls'"

export QT_QPA_PLATFORMTHEME=qt5ct

# fix delete
bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^U" backward-kill-line

source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
if [[ -v DISPLAY ]]; then
	cat ~/.cache/wal/sequences
    if [[ $(pgrep tmux:\ client) != "" ]]; then
        [ -z "${TMUX}" ] && (tmux attach || tmux)
    else
        [ -z "${TMUX}" ] && (tmux attach >/dev/null 2>&1 || tmux)
    fi
fi
