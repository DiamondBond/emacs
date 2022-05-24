<a href="https://www.gnu.org/software/emacs/"><img alt="GNU Emacs" src="https://github.com/minad/corfu/blob/screenshots/emacs.svg?raw=true"></a>

<img src="https://raw.githubusercontent.com/DiamondBond/emacs/master/img/gnusstorm-2.gif">


# Introduction

A sensible Emacs distro aimed at enhancing the default experience.


# Quick Start


## 0. Install Emacs

[Emacs](https://github.com/DiamondBond/emacs/blob/master/docs/emacsfromsource.org) build instructions.


## 1. Clone this repository


    git clone https://github.com/diamondbond/emacs ~/git/emacs


## 2. Install the configuration


    mv ~/.emacs.d ~/.emacs.d.old

    cd ~/git/emacs
    chmod +x install.sh
    ./install.sh


## 3. Bootstrap Emacs

This will take some time.
NOTE: Please answer y for vterm-modules to be autoinstalled.


    emacs --daemon


## 4. Connect to Emacs


    emacsclient -c -n -a 'emacs'


## 5. Post Setup

Once Emacs launches for the first time we need to setup a few more things for everything to work as intended.


### Compile pdf-tools

Install pdf-tools.

> M-x pdf-tools-install


### Compile vterm modules

Compile vterm-modules & setup vterm.

> M-x vterm


### Setup all-the-icons

Download & install the all-the-icons font.

> M-x all-the-icons-install-fonts


### Setup LSP

Install [language servers](https://github.com/emacs-lsp/lsp-mode#supported-languages) manually (see [dependencies](https://github.com/DiamondBond/emacs#dependencies)) or with `M-x lsp-install-server`


### Setup Email

Refer to the [Email setup documentation](https://github.com/DiamondBond/emacs/blob/master/docs/setupemail.org) for this functionality.


# Principles

This configuration and all associated modules intend to follow the below priniciples.


## Performant

Provides a responsive experience that adheres to form follows function principles.


## Reproducible

Utilizes straight.el for reproducibility & increased stability.


## Literate

Every elisp snippet is commented or documented.

Provide verbose doc-strings and inline code commentary.


## Correct

Aggressively utilizes best practices.

Where possible, we will leverage built-in Emacs functionality instead of external packages.


## Modular

The base configuration only sets up Emacs to have a cleaner presentation with sensible defaults.

The implication is that someone should be able to install or copy code from this configuration into their own configuration or vice versa.


# Modules

This list is in chronological load order.

-   **[early-bootstrap](early-init.el):** Early bootstrap
-   **[bootstrap](init.el):** Bootstrap straight.el
-   **[base](https://github.com/DiamondBond/emacs/blob/master/config.org#base):** Base configuration
-   **[keybinds](https://github.com/DiamondBond/emacs/blob/master/config.org#keybinds):** Keybinding maps
-   **[packages](https://github.com/DiamondBond/emacs/blob/master/config.org#use-package):** Package manifest
-   **[languages](https://github.com/DiamondBond/emacs/blob/master/config.org#languages):** Language manifest
-   **[functions](https://github.com/DiamondBond/emacs/blob/master/config.org#functions):** Function definitions
-   **[user](https://github.com/DiamondBond/emacs/blob/master/userconfig.org):** User configuration
-   **[mail](https://github.com/DiamondBond/emacs/blob/master/modules/mail.org):** Mail configuration


# Dependencies

Dependencies required to run this Emacs distro.


## Packages


### System

> offlineimap
> fzf ripgrep ag
> clang clangd llvm
> texlive-core texlive-bin texlive-science
> imagemagick
> editorconfig
> libjansson
> rust-all cargo
> nodejs npm
> libxml2
> gopls
> gnuplot
> prettier
> hunspell


### Python

    pip3 install pyflakes isort nose pytest pygifsicle nose2 httpserver future pandas numpy matplotlib python-rofi


### Node

Deno & typescript-language-server for Typescript (TS/TSX) support.

    sudo npm install -g deno typescript-language-server bash-language-server


## Files


### ~/org & ~/pdfs

Symlink ~/org & ~/pdf to their respective subdirs within ~/Dropbox.

    mkdir -p ~/Dropbox/{org,pdfs}; ln -s ~/Dropbox/org ~/org; ln -s ~/Dropbox/pdfs ~/pdfs


### auth-info

1.  ~/org/.authinfo.gpg
2.  ~/org/secrets-el.gpg
3.  ~/org/network-security.data

    NOTE: 'auth-info' can be backed up and restored to/from ~/org with `M-x
       sync/personal-backup` & `M-x sync/personal-restore`.


### Emacs related files

1.  ~/.emacs.d/places
2.  ~/.emacs.d/recentf
3.  ~/.emacs.d/custom.el

    NOTE: 'Emacs related files' can be backed up and restored to/from ~/org with `M-x
       sync/personal-backup` & `M-x sync/personal-restore`.
