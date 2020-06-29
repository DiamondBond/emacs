#!/bin/bash
echo "backing up old config..."
cp ~/.emacs ~/.emacs.old
echo "copying new config..."
cp .emacs ~/.emacs
echo "done! don't forget to M-x package-install evil"
