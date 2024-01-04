(defun join-path (path filename)
  "Concat path and file. Add '/' to the end of the path if necessary."
  (concat path (if (string-match-p "/$" path) "" "/") filename))

(defun load-if-exists (f)
  "Load file F if it exists."
  (if (file-exists-p (expand-file-name f))
	  (load-file (expand-file-name f))))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
	  (if (string-match (car my-pair) buffer-file-name)
		  (funcall (cdr my-pair)))))

(defun tab-bar-enable ()
  "Enable tab-bar with history."
  (interactive)
  (tab-bar-mode 1)
  ;;(tab-bar-history-mode 1)
  (put 'tab-bar-toggle 'state t))

(defun tab-bar-disable ()
  "Disable tab-bar."
  (interactive)
  (tab-bar-mode -1)
  (put 'tab-bar-toggle 'state nil))

(defun tab-bar-toggle ()
  "Toggle tab-bar."
  (interactive)
  (if (get 'tab-bar-toggle 'state)
	  (tab-bar-disable)
	(tab-bar-enable)))

(defun erc-start ()
  "Start ERC and connect to Rizon."
  (interactive)
  (save-current-buffer
	(erc-services-mode 1)
	(erc-update-modules)
	(erc :server "irc.rizon.net" :port "6667" :nick erc-nick-short)))

(defun erc-quit ()
  "Quit ERC."
  (interactive)
  (erc-services-mode 0)
  (erc-quit-server nil))

(defun kill-async-buffers ()
  "Kill all buffers matching '*Async Shell Command' regex."
  (interactive)
  (kill-matching-buffers "*Async Shell Command*" nil t))

(defun disable-all-themes ()
  "Disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
	(disable-theme i)))

(defun split-and-follow-horizontally ()
  "Split and follow horizontally."
  (interactive)
  (split-window-below)
  ;; (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Split and follow vertically."
  (interactive)
  (split-window-right)
  ;; (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun run-in-vterm-kill (process event)
  "A process sentinel.  Kill PROCESS's buffer if it is live with arg EVENT."
  (let ((b (process-buffer process)))
	(and (buffer-live-p b)
		 (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

	  Interactively, prompt for COMMAND with the current buffer's file
	  name supplied.  When called from Dired, supply the name of the
	  file at point.

	  Like `async-shell-command`, but run in a vterm for full terminal features.

	  The new vterm buffer is named in the form `*foo bar.baz*`, the
	  command and its arguments in earmuffs.

	  When the command terminates, the shell remains open, but when the
	  shell exits, the buffer is killed."
  (interactive
   (list
	(let* ((f (cond (buffer-file-name)
					((eq major-mode 'dired-mode)
					 (dired-get-filename nil t))))
		   (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
	  (read-shell-command "Command: "))))
  (with-current-buffer (vterm (concat "*" command "*"))
	(set-process-sentinel vterm--process #'run-in-vterm-kill)
	(vterm-send-string command)
	(vterm-send-return)))

(defun ncmpcpp ()
  (interactive)
  (run-in-vterm "ncmpcpp"))

(defun yank-whole-buffer ()
  "Yank whole buffer."
  (interactive)
  (save-excursion
	(mark-whole-buffer)
	(call-interactively 'evil-yank)))

(defun open-in-external-app ()
  "Open the file(s) at point with an external application."
  (interactive)
  (let ((file-list (dired-get-marked-files)))
	(mapc
	 (lambda (file-path)
	   (let ((process-connection-type nil))
		 (start-process "" nil "xdg-open" file-path)))
	 file-list)))

(defun consult-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
		   (fboundp 'w32-shell-execute))
	  (w32-shell-execute "open" file)
	(call-process (pcase system-type
					('darwin "open")
					('cygwin "cygstart")
					(_ "xdg-open"))
				  nil 0 nil
				  (expand-file-name file))))

(defun dired-open-externally (&optional arg)
  "Open marked or current file (ARG) in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (consult-file-externally (dired-get-filename))
   arg))

(defun open-treemacs ()
  "Open treemacs."
  (interactive)
  (treemacs)
  (other-window 1))

(if (eq system-type 'gnu/linux)
	(defun config/reload ()
	  "Reload this Emacs configuration."
	  (interactive)
	  (load-file (concat user-emacs-directory "init.el"))))

(defun config/github ()
  "Opens this configurations GitHub website."
  (interactive)
  (browse-url "https://github.com/diamondbond/emacs"))

(when (file-readable-p "~/bin/auth-backup.sh")
  (defun auth/backup ()
	"Backup auth."
	(interactive)
	(async-shell-command "~/bin/auth-backup.sh")))

(when (file-readable-p "~/bin/auth-restore.sh")
  (defun auth/restore ()
	"Restore auth."
	(interactive)
	(async-shell-command "~/bin/auth-restore.sh")))

(defun sync/irc ()
  "Connect to IRC."
  (interactive)
  (erc-start)
  (rcirc 1))

(defun get-date ()
  "Get date."
  (format-time-string "%b %d, %Y"))

(defun insert-date ()
  "Insert date."
  (interactive)
  (insert (get-date)))

(defun insert-org-link-template ()
  "Insert org link template at point."
  (interactive)
  (setq last-command-event 91)
  (org-self-insert-command 1)
  (setq last-command-event 91)
  (org-self-insert-command 1)
  (setq last-command-event 'right)
  (right-char 1)
  (setq last-command-event 91)
  (org-self-insert-command 1))

(defun insert-wild-notifier-template ()
  "Insert WILD_NOTIFIER_NOTIFY_BEFORE template at point."
  (interactive)
  (insert ":PROPERTIES:
:WILD_NOTIFIER_NOTIFY_BEFORE: 60 30 15 10 5
:END:"))

(defun insert-current-file-name-at-point (&optional full-path)
  "Insert the current filename at point.
With prefix argument, use FULL-PATH."
  (interactive "P")
  (let* ((buffer
		  (if (minibufferp)
			  (window-buffer
			   (minibuffer-selected-window))
			(current-buffer)))
		 (filename (buffer-file-name buffer)))
	(if filename
		(insert (if full-path filename (file-name-nondirectory filename)))
	  (error (format "Buffer %s is not visiting a file" (buffer-name buffer))))))

(defun latex-spacer ()
  "Inserts a LaTeX spacer org-export block at point."
  (interactive)
  (setq last-command-event 67108908)
  (org-insert-structure-template "export")
  (setq last-command-event 108)
  (org-self-insert-command 1)
  (setq last-command-event 97)
  (org-self-insert-command 1)
  (setq last-command-event 116)
  (org-self-insert-command 1)
  (setq last-command-event 101)
  (org-self-insert-command 1)
  (setq last-command-event 120)
  (org-self-insert-command 1)
  (setq last-command-event 13)
  (org-return nil nil 1)
  (setq last-command-event 92)
  (org-self-insert-command 1)
  (setq last-command-event 92)
  (org-self-insert-command 1)
  (setq last-command-event 126)
  (org-self-insert-command 1)
  (setq last-command-event 92)
  (org-self-insert-command 1))

(defun emacs-devel ()
  "Read the Emacs-devel mailing list."
  (interactive)
  (setq last-command-event 121)
  (gnus nil)
  (setq last-command-event 121)
  (execute-extended-command nil "gnus" "gnus")
  (setq last-command-event 13)
  (gnus-group-browse-foreign-server
   `(nntp "news.gmane.io"))
  (setq last-command-event 13)
  (consult-line)
  (setq last-command-event 13)
  (gnus-browse-select-group nil))

;; https://stackoverflow.com/questions/12014036/emacs-make-frame-switch-buffer
(defun get-buffer-menu-in-new-frame ()
  "Switch-to-buffer in new frame."
  (interactive)
  (switch-to-buffer (list-buffers-noselect)))

(defun shrink-wrapped-buffer-list ()
  "Launch frame-fitted *Buffer List*."
  (interactive)
  (switch-to-buffer (list-buffers-noselect))
  (shrink-wrap))

(defun unfill-paragraph ()
  "Unfill current paragraph."
  (interactive)
  (let ((fill-column (point-max)))
	(fill-paragraph nil)))

(defun unfill-region ()
  "Unfill current region."
  (interactive)
  (let ((fill-column (point-max)))
	(fill-region (region-beginning) (region-end) nil)))

(defun next-15-lines ()
  "Move to the next 15 lines."
  (interactive)
  (forward-line 15))

(defun previous-15-lines ()
  "Move to the previous 15 lines."
  (interactive)
  (forward-line -15))

(defun upcase-last-word ()
  "Convert last word to uppercase."
  (interactive)
  (move-end-of-line 1)
  (backward-word 1)
  (upcase-word 1)
  (move-beginning-of-line 1)
  (next-line 1 1))

(defun open-line-below ()
  "Open a new line below point."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Open a new line above point."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun db/delete-current-line ()
  "Kill the whole line on which point is."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun db/duplicate-line()
  "Duplicate line at point."
  (interactive)
  (save-excursion
	(move-beginning-of-line 1)
	(kill-line)
	(yank)
	(open-line 1)
	(forward-line 1)
	(yank)))

;; BIONIC reading mode by xahlee
(defvar infu-bionic-reading-face nil "a face for `infu-bionic-reading-region'.")
(setq infu-bionic-reading-face 'error)
;; try
;; 'bold
;; 'error
;; 'warning
;; 'highlight
;; or any value of M-x list-faces-display

(defun infu-bionic-reading-buffer ()
  "Bold the first few chars of every word in current buffer.
Version 2022-05-21"
  (interactive)
  (infu-bionic-reading-region (point-min) (point-max)))

(defun infu-bionic-reading-region (Begin End)
  "Bold the first few chars of every word in region.
Version 2022-05-21"
  (interactive "r")
  (let (xBounds xWordBegin xWordEnd  )
	(save-restriction
	  (narrow-to-region Begin End)
	  (goto-char (point-min))
	  (while (forward-word)
		;; bold the first half of the word to the left of cursor
		(setq xBounds (bounds-of-thing-at-point 'word))
		(setq xWordBegin (car xBounds))
		(setq xWordEnd (cdr xBounds))
		(setq xBoldEndPos (+ xWordBegin (1+ (/ (- xWordEnd xWordBegin) 2))))
		(put-text-property xWordBegin xBoldEndPos
						   'font-lock-face infu-bionic-reading-face)))))

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
	  If no region is selected and current line is not blank and we are not at the end of the line,
	  then comment current line.
	  Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
	  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
	(comment-dwim arg)))

;; Convert camel to snake_case
(defun camel-to-snake-case (arg)
  "Convert a camelCase word to snake_case.

If the prefix argument ARG is non-nil, convert the text to uppercase."
  (interactive "p")
  (progn
	(let ((start (region-beginning))
		  (end (region-end))
		  (case-fold-search nil)
		  (had-initial-underscore nil))
	  (goto-char start)
	  (when (looking-at "_") (setq had-initial-underscore t))
	  (while (re-search-forward "\\([A-Z]\\)" end t)
		(replace-match "_\\1")
		(setq end (1+ end)))
	  (if arg
		  (upcase-region start end)
		(downcase-region start end))
	  (goto-char start)
	  (unless had-initial-underscore (delete-char 1)))))

(defun sanemacs/backward-kill-word ()
  "Kill word backwards without littering 'kill-ring'."
  (interactive )
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(when (file-readable-p "~/bin/org-pdf-export")
  (defun org-export-pdf ()
	"Export as pdf using eisvogel LaTeX template."
	(lambda)
	(interactive)
	(org-latex-export-to-latex)
	(async-shell-command (concat "~/bin/org-pdf-export " buffer-file-name))
	(find-file (expand-file-name (concat (file-name-sans-extension buffer-file-name) ".pdf")))))

;; Reverse video mode
(defun reverse-video-mode ()
  "Reverse video mode."
  (interactive)
  (add-to-list 'default-frame-alist '(reverse . t)))

(defun remove-reverse-video-mode ()
  "Remove 'reverse . t' from `default-frame-alist`."
  (interactive)
  (when (assq 'reverse default-frame-alist)
	(setq default-frame-alist (assq-delete-all 'reverse default-frame-alist))))

;; Eshell functions
(defun eshell/clear-scrollback ()
  "Clear the scrollback content of the eshell window."
  (let ((inhibit-read-only t))
	(erase-buffer)))

(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
	  (progn
		(split-window-sensibly (selected-window))
		(other-window 1)
		(eshell))
	(switch-to-buffer-other-window "*eshell*")))

(defun dm-wrap-region ()
  "Wrap text region to 80 columns."
  (interactive)
  (if (use-region-p)
	  (let ((start (region-beginning))
			(end (region-end)))
		(save-excursion
		  (goto-char start)
		  (while (< (point) end)
			(let ((line-end (save-excursion (end-of-line) (point))))
			  (fill-region (point) line-end 80)
			  (goto-char line-end))))
		(message "Text wrapped to 80 columns in the selected region."))
	(message "No active region. Please select a region to wrap.")))

(provide 'functions)
;;; functions.el ends here
