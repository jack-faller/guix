#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
	[ -x "$f" ] && . "$f"
    done
    unset f
fi

setxkbmap gb -option "caps:escape,compose:menu"
xrdb ~/.config/Xresources
repaper &
xcompmgr &
redshift -l 50.721680:-1.878530 &
xset -b -dpms s off &
xsetroot -cursor_name left_ptr &
easyeffects &
discord &
transmission-gtk &
qutebrowser &
# Wait for transmission to start.
(sleep 5 && automatic -c ~/.config/automatic/automatic.conf -l /tmp/automatic.log) &
sxhkd &
exec bspwm
