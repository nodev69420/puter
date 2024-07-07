xrandr --output HDMI-1 --auto --primary --output eDP-1 --auto --right-of HDMI-1
setxkbmap gb
xset r rate 200 80
xbanish &
qtile start
