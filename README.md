<a href="https://www.gnu.org/software/emacs/"><img alt="GNU Emacs" src="https://github.com/minad/corfu/blob/screenshots/emacs.svg?raw=true"></a>

<img src="https://raw.githubusercontent.com/DiamondBond/emacs/master/img/gnusstorm-2.gif" align="right" width="50%">


# Introduction

Emacs distro aimed at enhancing the default experience.

It is recommend to run this distribution with the latest [Emacs HEAD](https://savannah.gnu.org/git/?group=emacs) compiled from source with
these [configure flags](https://github.com/DiamondBond/emacs/blob/master/README.org#configuration).


## Points of interest

[Main Configuration](https://github.com/DiamondBond/emacs/blob/master/config.org)

[Package Manifest](https://github.com/DiamondBond/emacs/blob/master/config.org#use-package)

[Keybind Maps](https://github.com/DiamondBond/emacs/blob/master/config.org#keybinds)

[Function Definitions](https://github.com/DiamondBond/emacs/blob/master/config.org#functions)


## Summary

This Emacs distro attempts to achieve the following goals:


### Performant in its nature of operation

Responsive & adheres to function over form principles.


### Aggressively utilizing best practices

Utilizes straight.el for reproducibility & increased stability.


### Extensively documented

Literate configuration with comments & inline docstrings.


### Contrived with alteration in mind

Simple to fork & edit


### Remain as simple as possible whilst maximizing usability

e.g: Vertico/Corfu & friends instead of Helm/Ivy/Company.


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


# Compile pdf-tools

    M-x pdf-tools-install


# Compile vterm modules

    M-x vterm


# Download & install all-the-icons font

    M-x all-the-icons-install-fonts


# Dependencies


## Packages

This section is just packages that I have needed in the past to comfortably run this Emacs distro, not everything here is required, this is just a rough guideline.


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

Please install deno & typescript-language-server for Typescript (TS/TSX) support.

    sudo npm install -g deno typescript-language-server bash-language-server


## Files

I use Dropbox and I symlink ~/org & ~/pdf to their respective subdirs within ~/Dropbox.

    mkdir -p ~/Dropbox/{org,pdfs}; ln -s ~/Dropbox/org ~/org; ln -s ~/Dropbox/pdfs ~/pdfs


## Emacs

How to compile and install the latest GNU Emacs @ HEAD.


### Grabbing sources

Clone the latest emacs sources:

    cd ~/git
    git clone -b master git://git.sv.gnu.org/emacs.git


### Configuration

Configure Emacs for building with json, native-comp and the athena toolkit with xaw3d toolbars.

    # Run the auto-generation script
    ./autogen.sh

    # Configure Emacs
    ./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-xft --with-xpm --with-gpm=no --disable-silent-rules --with-modules --with-file-notification=inotify --with-mailutils --with-x=yes --with-x-toolkit=athena --without-gconf --without-gsettings --with-lcms2 --with-imagemagick --with-xml2 --with-json --with-harfbuzz --without-compress-install --with-native-compilation --with-xinput2 CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer -flto -fno-semantic-interposition"
    # note: Remove "--with-xinput2" if you're building Emacs28


### Compiling

We build with all cores and natively compile everything ahead of time, this will take a while.

    make NATIVE_FULL_AOT=1 -j$(nproc)
    sudo make install


## Email


### 0. Prerequisites

mu4e (mu) & offlineimap to manage Email within Emacs.

> maildir=~/mail


### 1. Configuring offlineimap

[offlineimap.rc](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimaprc)

[offlineimap.py](https://github.com/DiamondBond/dotfiles/blob/master/.offlineimap.py)


### 2. Configuring mu

[mu Setup](https://github.com/DiamondBond/emacs/blob/master/config.org#prerequisites)
