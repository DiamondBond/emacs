#!/bin/sh

# X11
xset b off & # Disables bell
xset -dpms & # Disables Energy Star features
xset s off & # Disables screen saver

# Xresources
[[ -f ~/.Xresources ]] && xrdb ~/.Xresources & # Merge Xresources

# Disable touchscreen
xinput | grep 'ELAN224A' | grep 'pointer' | grep -Po 'id=\d+' | cut -d= -f2 | xargs xinput disable &

# Run the screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/exwm/desktop.el
