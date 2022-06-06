cd C:\Users\Diamond\.emacs.d\
emacs.exe --batch -l org "config.org" -f org-babel-tangle
emacs.exe --batch -l org "globals.org" -f org-babel-tangle
emacs.exe --batch -l org "userconfig.org" -f org-babel-tangle
emacs.exe --batch -l org "modules/evil.org" -f org-babel-tangle
emacs.exe --batch -l org "modules/reddit.org" -f org-babel-tangle
emacs.exe --batch -l org "modules/discord.org" -f org-babel-tangle
emacs.exe --batch -l org "modules/archive/desktop/desktop.org" -f org-babel-tangle
