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
This is a simple configuration file for [GNU Emacs]

# Features
- VI modelines via eVIl
- Inhibited startup message
- Select to clipboard
- Conservative scrolling
- Inline org-mode images
- Nicer eshell prompt
- Bracket completion
- Some aliases (open, clean & y-or-n)
- Some keybindings:
  - Super + Ctrl + Return: Open eShell
  - Shift + Ctrl + Direction: Shrink or enlarge windows
  - F5: toggles menubar
  - F7: toggles scrollbar
  - F12: toggles line numbers
  - Ctrl + F1: loads modus-operandi theme
  - Ctrl + F2: loads modus-vivendi theme

# Prerequisites
+ Git (sudo apt/dnf install git)
+ Emacs (sudo apt/dnf install emacs)
+ MELPA
  + M-x package-install RET evil
  + M-x package-install RET modus-operandi
  + M-x package-install RET modus-vivendi

# Install
``` sh
git clone https://github.com/diamondbond/emacs ~/git/emacs
chmod +x ~/git/emacs/install.sh
~/git/emacs/install.sh
```
