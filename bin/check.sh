#!/bin/sh
# Boot this config in a throwaway terminal Emacs and fail on init errors or warnings.
# Waits past the longest :defer so deferred :config errors show up.
# The first run after adding a package is slow: straight clones it.
out=$(mktemp)
timeout 600 script -qc "emacs -nw --eval '(add-hook (quote emacs-startup-hook) (lambda () (run-at-time 6 nil (lambda () (with-temp-file \"$out\" (insert (format \"init-error: %s\ninit-time: %s\n\" init-file-had-error (emacs-init-time))) (dolist (b (list \"*Warnings*\" \"*Messages*\")) (when (get-buffer b) (insert \"== \" b \"\n\" (with-current-buffer b (buffer-string)))))) (setq kill-emacs-hook nil) (kill-emacs)))))'" /dev/null >/dev/null 2>&1
cat "$out"
! { grep -q '^init-error: t' "$out" || sed 1,2d "$out" | grep -qE '^== \*Warnings\*|[Ee]rror'; }
