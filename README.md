# Diamond's Emacs

An Emacs 31 config that sits next to Claude Code: evil, vertico, consult, corfu, magit, vterm, eglot and tree-sitter. Small, and built-in wherever Emacs allows.

## Install

```sh
git clone https://github.com/DiamondBond/emacs ~/.emacs.d
```

straight.el installs packages on first start; tree-sitter grammars install on first use.

## Needs

- Emacs 31
- A Nerd Font for icons: `SauceCodePro Nerd Font Mono`, or set `nerd-icons-font-family`
- Language servers: `clangd`, `gopls`, `rust-analyzer`, `typescript-language-server`
- `~/org` for notes, agenda and scratch (optional)

## Layout

```
early-init.el       startup tuning
init.el             straight.el, globals, loads lisp/
lisp/base.el        built-in settings
lisp/keybinds.el    C-1 prefix map, global keys
lisp/packages.el    packages
lisp/languages.el   eglot, tree-sitter, language modes
lisp/functions.el   personal commands
bin/check.sh        boots a throwaway Emacs, fails on errors or warnings
```
