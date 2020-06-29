<div align="center">

# Diamond's Emacs
Minimal emacs configuration.

[Install](#install)

![Screenshot](https://raw.githubusercontent.com/DiamondBond/emacs/master/img/emacs.png)

</div>

---

### Table of Contents
- [Introduction](#introduction)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Install](#install)

# Introduction
This is a configuration file for [GNU Emacs] with the aim to make emacs easier
to use for the VIM convert.
It is lightweight and only relies on the EVIL Melpa package.

# Features
- Mostly default emacs appearance with a few GUI tweaks
- VI modelines (via eVIl mode - make sure to M-x package-install evil)
- Misc QOL improvements to various things within emacs (see .emacs for more details)
  Heres a few notable mentions:
  - Inhibited startup message
  - Pretty eshell
  - Sane indentation
  - Bracket completion
  - A few simple aliases (open, clean & y-or-n)
- Nifty keybinds
  - F12: toggles line numbers
  - Shift + Ctrl + Direction: Shrink or enlarge windows
  - Super + Ctrl + Return: Open eShell

# Prerequisites
+ Git (sudo apt/dnf install git)
+ Emacs (sudo apt/dnf install emacs)
+ MELPA
+ EVIL (M-x package-install evil)

# Install
``` sh
git clone https://github.com/diamondbond/emacs ~/git/emacs
chmod +x ~/git/emacs/install.sh
~/git/emacs/install.sh
```
