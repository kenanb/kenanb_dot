#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xbindkeys
setxkbmap -layout tr -option ctrl:swapcaps
xsetwacom --set 8 MapToOutput "1440x900+0+0"
./.fehbg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
exec i3
