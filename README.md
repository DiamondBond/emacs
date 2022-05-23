<a href="https://www.gnu.org/software/emacs/"><img alt="GNU Emacs" src="https://github.com/minad/corfu/blob/screenshots/emacs.svg?raw=true"></a>

<img src="https://raw.githubusercontent.com/DiamondBond/emacs/master/img/gnusstorm-2.gif">


# Introduction

A sensible Emacs distro aimed at enhancing the default experience.


# Quick Start

Install the latest [Emacs@HEAD](https://github.com/DiamondBond/emacs/blob/master/README.org/#compiling) compiled from source with these [configure flags](https://github.com/DiamondBond/emacs/blob/master/README.org#configuration).

Clone this repository to `~/.emacs.d` or `~/.config/emacs`:


    git clone https://github.com/diamondbond/emacs ~/.emacs.d


# Principles

This configuration and all associated modules intend to follow the below priniciples.


## Performant

Responsive & adheres to function over form principles.


## Reproducible

Utilizes straight.el for reproducibility & increased stability.


## Literate

Comments & inline doc-strings.


## Correct

Aggressively utilizes best practices.

Where possible, we will leverage built-in Emacs functionality instead of external packages.


## Modular

The base configuration only sets up Emacs to have a cleaner presentation with sensible defaults.
It is up to the user to decide how to customize it.

Configuration modules should depend on other modules and the base configuration as little as possible. When a configuration module needs to integrate with other functionality in Emacs, the standard extensibility points of each package should be used (instead of expecting our own configuration module).

The implication is that someone should be able to install or copy code from this configuration into their own configuration without using this Emacs distro.


# Modules

This list is in chronological load order.

-   **[early-bootstrap](early-init.el):** Early bootstrap
-   **[bootstrap](init.el):** Bootstrap straight.el
-   **[base](https://github.com/DiamondBond/emacs/blob/master/config.org#base):** Base configuration
-   **[packages](https://github.com/DiamondBond/emacs/blob/master/config.org#use-package):** Package manifest
-   **[keybinds](https://github.com/DiamondBond/emacs/blob/master/config.org#keybinds):** Keybinding maps
-   **[functions](https://github.com/DiamondBond/emacs/blob/master/config.org#functions):** Function definitions


# Post Setup

Once Emacs launches for the first time we need to setup a few more things for everything to work correctly.


## Compile pdf-tools

Install pdf-tools.

> M-x pdf-tools-install


## Compile vterm modules

Compile vterm-modules & setup vterm.

> M-x vterm


## Setup all-the-icons

Download & install the all-the-icons font.

> M-x all-the-icons-install-fonts


## Setup Email

mu4e (mu) & offlineimap to manage Email within Emacs.


### 0. Prerequisites

Create your maildir in $HOME as ~/mail

    mkdir -p ~/mail


### 1. Configuring offlineimap

~/[offlineimap.rc](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimaprc)

[~/offlineimap.py](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimap.py)


### 2. Configuring mu

Please refer to [mu Setup](https://github.com/DiamondBond/emacs/blob/master/config.org#prerequisites) in config.org.

-   Compile mu

    Compile mu & mu4e.

    > M-x mu-setup/build-mu-binary

-   Initialize mu

    Initialize mu with maildir & address.

    > M-x mu-setup/init-mu

-   Build mu index

    Inform mu as to the contents of ~/mail by building/rebuilding the mu index.

    > M-x mu-setup/rebuild-mu-index


# Dependencies


## Packages


### System

> offlineimap
> fzf ripgrep ag
> clang clangd llvm
> texlive-latex-recommended
> imagemagick
> editorconfig
> libjansson
> rust-all cargo
> nodejs npm
> libxml2
> gopls
> prettier


### Python

    pip3 install pyflakes isort nose pytest pygifsicle nose2 httpserver future pandas numpy matplotlib python-rofi


### NPM

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


### Emacs related files

1.  ~/.emacs.d/places
2.  ~/.emacs.d/recentf
3.  ~/.emacs.d/custom.el


## Emacs

How to compile and install the latest GNU Emacs @ HEAD.


### Grabbing sources

Clone the latest emacs sources:

    cd ~/git
    git clone -b master git://git.sv.gnu.org/emacs.git


### Configuration

Configure Emacs with json, native-comp, xinput2 and the athena toolkit with xaw3d-toolbars.

    # Run the auto-generation script
    ./autogen.sh

    # Configure Emacs
    ./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-xft --with-xpm --with-gpm=no --disable-silent-rules --with-modules --with-file-notification=inotify --with-mailutils --with-x=yes --with-x-toolkit=athena --without-gconf --without-gsettings --with-lcms2 --with-imagemagick --with-xml2 --with-json --with-harfbuzz --without-compress-install --with-native-compilation --with-xinput2 CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer -flto -fno-semantic-interposition"
    # note: Remove "--with-xinput2" if your target build is Emacs28


### Compiling

Compile with all cores and native full ahead of time compilation, this takes a while.

    make NATIVE_FULL_AOT=1 -j$(nproc)
    sudo make install
