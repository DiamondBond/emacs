#!/bin/sh

# X11
xset b off & # Disables bell
xset -dpms & # Disables Energy Star features
xset s off & # Disables screen saver

# Xresources
[[ -f ~/.Xresources ]] && xrdb ~/.Xresources & # Merge Xresources

# Run the screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm -l ~/.emacs.d/exwm/desktop.el
