#!/usr/bin/sh

setxkbmap gb
xset r rate 200 80
# xrdb -merge ~/.Xresources
xsetroot -cursor_name miku-cursor-linux
xrandr --output HDMI-1 --primary --output HDMI-2 --right-of HDMI-1
