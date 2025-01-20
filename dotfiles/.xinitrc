# xhost +SI:localuser:$USER
export _JAVA_AWT_WM_NONREPARENTING=1

setxkbmap gb
xset r rate 200 80

picom &
feh --bg-fill ~/puter/pape/pape.jpg
xrdb ~/.Xresources &
xsetroot -cursor_name left_ptr &

# exec emacs -mm --debug-init
exec i3
