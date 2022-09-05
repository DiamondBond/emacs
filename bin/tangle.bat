cd C:\Users\Diamond\.emacs.d\
emacs.exe --batch -l org "config.org" -f org-babel-tangle
emacs.exe --batch -l org "userconfig.org" -f org-babel-tangle
emacs.exe --batch -l org "modules/evil.org" -f org-babel-tangle
