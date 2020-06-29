<div align="center">

# Diamond's Emacs
Minimal emacs configuration.

[Install](#install)

![Screenshot](https://raw.githubusercontent.com/diamondbond/emacs/img/emacs.png)

</div>

---

### Table of Contents
- [Introduction](#introduction)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Install](#install)

# Introduction
This is a configuration file for [GNU Emacs] aimed at being simple and easy to
use for the VIM convert.
It is lightweight and only relies on a few MELPA packages.

# Features
- Mostly default emacs appearance with a few GUI tweaks (notably; hidden toolbar)
- Misc QOL improvements to various things within emacs
- Inhibited startup message
- Pretty eshell
- Sane indentation
- Bracket completion
- A few simple aliases (open, clean & y-or-n)
- VI modelines (via eVIl mode - make sure to M-x package-install evil)
- Nifty keybinds
  - F12: toggles line numbers
  - Shift + Ctrl + Direction: Shrink or enlarge windows
  - Super + Ctrl + Return: Open eShell

# Prerequisites
+ Git
+ Emacs
+ MELPA
+ EVIL

# Install
``` sh
git clone https://github.com/diamondbond/emacs ~/git/emacs
chmod +x ~/git/emacs/install.sh
~/git/emacs/install.sh
```
