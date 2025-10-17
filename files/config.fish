function fish_right_prompt
    set -q GUIX_ENVIRONMENT && echo -n "[guix shell]"
end
if status is-interactive
    fish_vi_key_bindings
    set -x EDITOR e
    set -x VISUAL "$EDITOR"
    set -x DOTNET_CLI_TELEMETRY_OPTOUT 1
    alias off="sudo $(realpath $(which halt))"
    alias reboot="sudo $(realpath $(which reboot))"
    alias glances="glances -1"
    alias please="sudo"
    alias tm="mv --verbose --backup --target-directory ~/trash"
    alias pass="EDITOR='vim' pass"
    if set -q DISPLAY
	cat ~/.cache/wal/sequences
	if ! set -q TMUX
	    tmux attach || tmux
	end
    end
end
if status is-login && [ "$XDG_VTNR" = 1 ]
     ~/.config/start-wm
end
