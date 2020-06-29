<div align="center">

# Diamond Emacs
My personal minimal emacs configuration.

[Install](#install) • [Documentation] • [FAQ] • [Screenshots]

![Doom Emacs Screenshot](https://raw.githubusercontent.com/diamondbond/emacs/img/emacs.png)

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
- Nifty keybinds
  - F12: toggles line numbers
  - Shift + Ctrl + Direction: Shrink or enlarge windows
  - Super + Ctrl + Return: Open eShell
- Bracket completion
- Inhibited startup message
- Pretty eshell
- Sane indentation
- A few simple aliases (open, clean, y-or-n)
- VI modelines (via eVIl mode - make sure to M-x package-install evil)

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
