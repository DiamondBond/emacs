Link:
```
sudo ln -f ~/.emacs.d/exwm/exwm.desktop /usr/share/xsessions/exwm.desktop
sudo systemctl restart gdm
```

Dependencies:
```
arandr
xss-lock
nm-applet
pasystray
blueman
picom
dunst
mpd mpc ncmpcpp
```

DE Deps:
https://github.com/DamienCassou/desktop-environment#configuration
```
amixer
brightnessctl
scrot
slock
playerctl
```

Ubuntu:
```
sudo apt install -y arandr xss-lock network-manager pasystray blueman picom dunst mpd mpc ncmpcpp alsa-utils brightnessctl scrot slock playerctl
```

```
cp ~/.emacs.d/exwm/dunstrc ~/.config/dunst/dunstrc
cp ~/.emacs.d/exwm/picom.conf ~/.config/picom.conf
```
