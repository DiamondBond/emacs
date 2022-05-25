#!/bin/bash
# Make emacs.d
mkdir -p ~/.emacs.d/;

# Install
cp init.el ~/.emacs.d/; cp early-init.el ~/.emacs.d/; cp config.org ~/.emacs.d/; cp userconfig.org ~/.emacs.d/

# Copy libs
cp -r modules ~/.emacs.d/; cp -r resources ~/.emacs.d/; cp -r snippets ~/.emacs.d/; cp -r img ~/.emacs.d/;
