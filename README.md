
# Table of Contents

-   [Introduction](#org2c07e14)
    -   [Contents](#org000623e)
    -   [Summary](#org0c32523)
        -   [Performance over all else](#orgbe0f245)
        -   [Aggressively use best practices](#orgfb230da)
        -   [Extensively document and comment all code](#orgcbbb824)
        -   [Remain as simple as possible whilst maximizing usability](#orgc9e5945)
-   [Setup](#org8fd8cbf)
-   [Compile pdf-tools](#org7da94e7)
-   [Compile vterm modules](#org6111d53)
-   [Download & install all-the-icons font](#org4979efe)
-   [Dependencies](#org657592a)
    -   [Packages](#orgfa33b13)
        -   [System](#orgb8adff4)
        -   [Python](#orge53b0d1)
        -   [NPM](#orgf4c3930)
    -   [Files](#orga3a361b)
    -   [Emacs](#org3908cec)
        -   [Grabbing sources](#org89c5399)
        -   [Configuration](#org81437f0)
        -   [Compiling](#orgce50cc9)
    -   [Email](#org95313c4)
        -   [0. Prerequisites](#orgb60d4a2)
        -   [1. Configuring offlineimap](#org3cc00ac)
        -   [2. Configuring mu](#orgfc69cd9)

<a href="https://www.gnu.org/software/emacs/"><img alt="GNU Emacs" src="https://github.com/minad/corfu/blob/screenshots/emacs.svg?raw=true"/></a>

<img src="https://raw.githubusercontent.com/DiamondBond/emacs/master/img/gnusstorm-2.gif" align="right" width="42%">


<a id="org2c07e14"></a>

# Introduction

Emacs distro aimed at enhancing the default experience.

It is recommend to run this distribution with the latest [Emacs HEAD](https://savannah.gnu.org/git/?group=emacs) compiled from source with
these [configure flags](https://github.com/DiamondBond/emacs/blob/master/README.org#configuration).


<a id="org000623e"></a>

## Contents

[Main Configuration](https://github.com/DiamondBond/emacs/blob/master/config.org)

[Package Manifest](https://github.com/DiamondBond/emacs/blob/master/config.org#use-package)

[Keybind Maps](https://github.com/DiamondBond/emacs/blob/master/config.org#keybinds)

[Function Definitions](https://github.com/DiamondBond/emacs/blob/master/config.org#functions)


<a id="org0c32523"></a>

## Summary

This Emacs distro attempts to achieve the following goals:


<a id="orgbe0f245"></a>

### Performance over all else

Function-over-form.


<a id="orgfb230da"></a>

### Aggressively use best practices

We utilize use-package & straight.el for reproducibility.


<a id="orgcbbb824"></a>

### Extensively document and comment all code

The main configuration is literate and has comments / docstrings wherever possible.


<a id="orgc9e5945"></a>

### Remain as simple as possible whilst maximizing usability

e.g: Vertico/Corfu & friends instead of Helm/Ivy/Company.


<a id="org8fd8cbf"></a>

# Setup

Please refer to the dependencies section below first to make sure Emacs and your environment have the required packages and configurations setup correctly, then proceed to clone and install this distro.

    # Clone configuration to ~/git/emacs
    mkdir -p ~/git; git clone https://github.com/diamondbond/emacs ~/git/emacs

    # Backup old emacs configuration
    mv ~/.emacs.d ~/.emacs.d.old

    # Copy new configuration to ~/.emacs.d
    mkdir -p ~/.emacs.d; cp -r ~/git/emacs/{early-init.el,init.el,config.org,snippets,img} ~/.emacs.d/

    # Start the Emacs daemon
    emacs --daemon;
    # make & get a cup/pot of tea &/or coffee,
    # straight.el will begin cloning all the declared packages.

    # Start Emacs client once straight.el is done
    emacsclient -c -n -a 'emacs'

The first time you run Emacs, straight.el will download all the declared packages in the config.org file, please be patient.

To get autocompletion on JS/TS & Python files, you'll need to install the corresponding lsp backend servers first (you'll be automatically prompted to do so the first time you open a relevant file).

**Post Setup**
Once Emacs launches for the first time we need to setup a few more things:


<a id="org7da94e7"></a>

# Compile pdf-tools

    M-x pdf-tools-install


<a id="org6111d53"></a>

# Compile vterm modules

    M-x vterm


<a id="org4979efe"></a>

# Download & install all-the-icons font

    M-x all-the-icons-install-fonts


<a id="org657592a"></a>

# Dependencies


<a id="orgfa33b13"></a>

## Packages

This section is just packages that I have needed in the past to comfortably run this Emacs distro, not everything here is required, this is just a rough guideline.


<a id="orgb8adff4"></a>

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


<a id="orge53b0d1"></a>

### Python

    pip3 install pyflakes isort nose pytest pygifsicle nose2 httpserver future pandas numpy matplotlib python-rofi


<a id="orgf4c3930"></a>

### NPM

Please install deno & typescript-language-server for Typescript (TS/TSX) support.

    sudo npm install -g deno typescript-language-server bash-language-server


<a id="orga3a361b"></a>

## Files

I use Dropbox and I symlink ~/org & ~/pdf to their respective subdirs within ~/Dropbox.

    mkdir -p ~/Dropbox/{org,pdfs}; ln -s ~/Dropbox/org ~/org; ln -s ~/Dropbox/pdfs ~/pdfs


<a id="org3908cec"></a>

## Emacs

How to compile and install the latest GNU Emacs @ HEAD.


<a id="org89c5399"></a>

### Grabbing sources

Clone the latest emacs sources:

    cd ~/git
    git clone -b master git://git.sv.gnu.org/emacs.git


<a id="org81437f0"></a>

### Configuration

Configure Emacs for building with json, native-comp and the athena toolkit with xaw3d toolbars.

    # Run the auto-generation script
    ./autogen.sh

    # Configure Emacs
    ./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-xft --with-xpm --with-gpm=no --disable-silent-rules --with-modules --with-file-notification=inotify --with-mailutils --with-x=yes --with-x-toolkit=athena --without-gconf --without-gsettings --with-lcms2 --with-imagemagick --with-xml2 --with-json --with-harfbuzz --without-compress-install --with-native-compilation --with-xinput2 CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer -flto -fno-semantic-interposition"
    # note: Remove "--with-xinput2" if you're building Emacs28


<a id="orgce50cc9"></a>

### Compiling

We build with all cores and natively compile everything ahead of time, this will take a while.

    make NATIVE_FULL_AOT=1 -j$(nproc)
    sudo make install


<a id="org95313c4"></a>

## Email


<a id="orgb60d4a2"></a>

### 0. Prerequisites

mu4e (mu) & offlineimap to manage Email within Emacs.

> maildir=~/mail


<a id="org3cc00ac"></a>

### 1. Configuring offlineimap

[offlineimap.rc](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimaprc)

[offlineimap.py](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimap.py)


<a id="orgfc69cd9"></a>

### 2. Configuring mu

[mu Setup](https://github.com/DiamondBond/emacs/blob/master/config.org#prerequisites)
