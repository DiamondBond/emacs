;; Apply globals
(setq user-full-name globals--name
	  user-mail-address globals--email
	  erc-nick globals--erc
	  erc-nick-short globals--irc
	  rcirc-default-user-name globals--irc
	  rcirc-default-nick      globals--irc
	  rcirc-default-full-name globals--name)

;; Load authinfo
(if (file-exists-p globals--auth-info)
	(use-package auth-source
	  :no-require t
	  :config (setq auth-sources globals--auth-sources))
  (setq auth-source-cache-expiry nil))

;; Ask for encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Email to encrypt to
(setq epa-file-encrypt-to '(globals--email))

;; Set GC threshold
(setq lsp-cons-threshold 100000000)

;; Do not GC during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Garbage collection minibuffer hack
(defun my-minibuffer-setup-hook ()
  "Garbage collection will never occur."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Garbage collection will kick off immediately."
  (setq gc-cons-threshold lsp-cons-threshold)) ;; or gc-cons-threshold-original

;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Disable warnings
(setq warning-suppress-types '((comp)))
(setq ad-redefinition-action 'accept)

;; Disable auto-window-vscroll
(setq auto-window-vscroll nil)

;; Set initial major modes
(setq-default initial-major-mode 'fundamental-mode
			  default-major-mode 'text-mode)

;; Disable confirmation on visiting a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Set default custom file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Write to it if it does not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)

;; Bookmark settings
(when (file-directory-p "~/org")
  (if (file-exists-p "~/org/bookmarks")
	  (setq bookmark-default-file "~/org/bookmarks")))
;; Hide bookmark marker in fringe
(setq bookmark-set-fringe-mark nil)
;; Save bookmarks everytime they are modified
(setq bookmark-save-flag 1)

;; Load any custom themes
(when (file-exists-p (expand-file-name "themes/" user-emacs-directory))
  (setq custom-safe-themes t)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory)))

;; Enable vc symlinks
(setq vc-follow-symlinks t)

;; Kill buffer settings
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
;; Prevent 'Buffer <name> modified; kill anyway?'
;; (add-hook 'kill-buffer-query-functions
;; 		  (lambda () (not-modified) t))
;; Not recommended - potentially causes slowdowns.

;; Disable backups
(setq-default backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq vc-make-backup-files nil)

;; Delete whitespace on save
(add-hook 'before-save-hook
		  'delete-trailing-whitespace)

;; UTF-8 rules
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set default dirs
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; Environment settings
(setenv "PAGER" "cat")
(setenv "GPG_AGENT_INFO" nil)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Disable motion events from mouse
(setq-default track-mouse nil)
;; (mouse-avoidance-mode 'jump)

;; Configure scrolling
;; If the frame contains multiple windows, scroll the one under the cursor
;; instead of the one that currently has keyboard focus.
(setq mouse-wheel-follow-mouse 't
	  ;; Completely disable mouse wheel acceleration to avoid speeding away.
	  mouse-wheel-progressive-speed nil)

;; Scroll wheel amount
;; mouse-wheel-scroll-amount '(2 ((shift) . 8) ((control) . 6)))

;; Scroll margin
(setq scroll-margin 0)
;; (setq scroll-conservatively 100000)

;; Fast but imprecise scrolling
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)

;; Enable pixel-scroll-precision-mode (emacs29+)
(pixel-scroll-precision-mode)

;; Clipboard settings
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; Auto-select window on hover (Focus follows mouse)
;; (setq mouse-autoselect-window t)

;; Enable & configure show-paren-mode
(setq show-paren-delay 0.1
	  show-paren-style 'parenthesis
	  show-paren-highlight-openparen t
	  show-paren-when-point-inside-paren t
	  show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;; Tabs & indents settings (highly controversial)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-line-function 4)
(setq-default lisp-indent-offset nil)
(setq-default sgml-basic-offset 4)
(setq-default electric-indent-inhibit nil)
(setq backward-delete-char-untabify-method 'nil)
(electric-indent-mode -1) ;; Disable electric indentation

;; C mode tabs/indents settings
(setq c-default-style "linux")
(setq c-basic-offset tab-width)

;; Enable subword mode in C mode
;; (global-subword-mode t)
(add-hook 'c-mode-common-hook
		  (lambda () (subword-mode 1)))

;; Disable double space sentence end
(setq sentence-end-double-space nil)

;; Set font
;; (when (string= (system-name) "matebook")
;; 	 (setq globals--font "Menlo 14"))
(add-to-list 'default-frame-alist `(font . ,globals--font))

;; Set time format
(setq-default display-time-format "%I:%M %p")

;; Set fill column width
(setq-default fill-column 80)

;; Show trailing whitespace in prog-modes
(add-hook 'prog-mode-hook
		  (lambda ()
			(setq show-trailing-whitespace t)))

;; Set frame title format
(setq-default frame-title-format '("%b"))

;; Make buffer names unique and easy to read
(setq-default uniquify-buffer-name-style 'forward)

;; Set internal border width to 0
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Enable pixelwise resizing
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Inhibit startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Stretch cursor
;; (setq x-stretch-cursor t)

;; Menu-bar
(if (fboundp 'menu-bar-mode)
	(menu-bar-mode 1))

;; Tool-bar
(if (fboundp 'tool-bar-mode)
	(tool-bar-mode 0))
;;(setq tool-bar-style 'image)

;; Scroll-bar
(if (fboundp 'scroll-bar-mode)
	(scroll-bar-mode 1))

;; for athena
;; (set-scroll-bar-mode 'right)

;; disable minibuffer scroll-bar
;; (set-window-scroll-bars (minibuffer-window) nil nil)

;; Tab-bar
;; (tab-bar-mode 1)
;; (setq tab-bar-show t)
;; (tab-bar-history-mode 1)
(setq tab-bar-history-limit 25)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
;; (setq tab-bar-new-tab-choice "*GNU Emacs*")
;; (setq tab-bar-close-button-show nil)
;; (setq tab-bar-new-button-show nil)

;; Enable tab-bar & local scroll-bar when using emacsclient
;; (use-package emacs
;;   :hook (server-after-make-frame . tab-bar-enable)
;;   :hook (server-after-make-frame . enable-local-scroll-bar))

;; Time in tab-bar
;; (display-time-mode 1)
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setq tab-bar-format '(tab-bar-format-global)
;; 	  tab-bar-mode t)

;; Configure fringe
;;(fringe-mode '(8 . 0))
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newline-into-fringe t)
;; Set fringe color
;; (set-face-attribute 'fringe nil :background "#ffffff" :foreground "#ffffff")

;; Set default frame-size
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 46))

;; Set default frame background
;;(add-to-list 'default-frame-alist '(background-color . "honeydew"))

;; Disable vsync
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Reverse video mode
;;(reverse-video-mode)

;; Configure some modes
(column-number-mode 1)
(global-hl-line-mode 0)
(global-prettify-symbols-mode t)

;; Line number modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Visual line mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Aliases
(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'third 'caddr)
(defalias 'when-not 'unless)
(defalias 'word-count 'count-words)
(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'shrink-wrap 'fit-frame-to-buffer)

;;(defalias 'recentf-delete-list 'recentf-edit-list)
(defalias 'bookmark-delete-all 'bookmark-delete)

(defalias 'sync/news 'elfeed-update)
(defalias 'sync/work 'ejira-update-my-projects)

;; Set browser depending on system
(if (eq system-type 'gnu/linux)
	(setq browse-url-browser-function globals--browser))
(if (eq system-type 'darwin)
	(setq browse-url-browser-function globals--browser))

;; Proced settings
(setq proced-auto-update-flag t)
(setq proced-auto-update-interval 5)
(setq proced-descend t)
(setq proced-filter 'user)

;; Set eshell prompt
(setq eshell-prompt-function
	  (lambda nil
		(concat
		 (if (string= (eshell/pwd) (getenv "HOME"))
			 (propertize "~" 'face `(:foreground "#2255bb"))
		   (replace-regexp-in-string
			(getenv "HOME")
			(propertize "~" 'face `(:foreground "#2255bb"))
			(propertize (eshell/pwd) 'face `(:foreground "#2255bf"))))
		 (if (= (user-uid) 0)
			 (propertize " α " 'face `(:foreground "#aa0000"))
		   (propertize " λ " 'face `(:foreground "#68228b"))))))
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")

;; Do not highlight prompt
(setq eshell-highlight-prompt nil)

;; Kill eshell zombie processes
(setq eshell-kill-processes-on-exit t)

;; Disable global highlight
(add-hook 'eshell-mode-hook
		  (lambda () (hl-line-mode 0)))

;; Eshell aliases
(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

(provide 'base)
;;; base.el ends here
