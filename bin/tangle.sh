#!/bin/bash

# tangle function
function org-tangle () {
	emacs --batch -l org "$1" -f org-babel-tangle
}

# set cwd
cd ~/.emacs.d/

# tangle base configuration
org-tangle config.org
org-tangle globals.org
org-tangle userconfig.org

# tangle modules
org-tangle modules/evil.org
org-tangle modules/mail.org
org-tangle modules/reddit.org
org-tangle modules/discord.org
#org-tangle modules/honeydew.org
org-tangle modules/archive/laptop/laptop.org
org-tangle modules/archive/desktop/desktop.org

if [ $# -eq 1 ]
  then
    source ~/bin/auth-restore.sh
    echo "arguments supplied, restored authinfo backup."
fi


# exit
exit 0
