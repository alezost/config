#!/bin/sh

xset r rate 193 50 b off
xsetroot -solid gray25 -xcf $HOME/.icons/default/cursors/cell 1
setxkbmap us,ru,us dvorak,,
# xmodmap $HOME/config/X/Xmodmap
xkbcomp -w 0 $HOME/config/X/keymap.xkb $DISPLAY
xrdb -merge $HOME/config/X/Xresources

# stumpwm &
exec xterm
