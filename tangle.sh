#!/bin/bash

# function to tangle .org files
function org-tangle () {
  emacs --batch -l org "$1" -f org-babel-tangle
}

# tangle base configuration
org-tangle config.org
org-tangle globals.org
org-tangle userconfig.org

# tangle modules
org-tangle modules/reddit.org
org-tangle modules/discord.org
org-tangle modules/mail.org
