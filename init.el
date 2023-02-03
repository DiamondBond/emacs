;;; init.el --- Initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Diamond Bond
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Diamond Bond <diamondbond1@gmail.com>
;; URL: https://github.com/diamondbond/emacs
;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; Utilitarian Emacs Configuration.

;;; Code:

;;---------------------------------------------------------------------
;; CORE
;;---------------------------------------------------------------------

;; Make emacs startup faster
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  "Revert file name handler alist."
  (setq file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;; For performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq process-adaptive-read-buffering nil)

;; Load newer .elc or .el
(setq load-prefer-newer t)

;; Initialize melpa repo
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Define when to check for package modifications,
;; for improved straight.el startup time.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package with straight.el
(straight-use-package 'use-package)

;; Configure straight.el
(use-package straight
  :custom
  (straight-check-for-modifications '(check-on-save find-when-checking))
  (straight-use-package-by-default t)
  (straight-vc-git-default-clone-depth 1)
  (straight-recipes-gnu-elpa-use-mirror t)
  (straight-check-for-modifications nil))

;;---------------------------------------------------------------------
;; GLOBALS
;;---------------------------------------------------------------------

(setq
 globals--banner-path  "img/gnusstorm-2.gif"                ; Banner
 globals--font         "Menlo 11"                           ; Font
 globals--name         "Diamond Bond"                       ; Name
 globals--email        "diamondbond1@gmail.com"             ; Email
 globals--irc          "diamondbond"                        ; IRC
 globals--erc          '("diamondbond" "diamondbond_")      ; ERC
 globals--auth-info    "~/.authinfo.gpg"                    ; Auth Info
 globals--auth-sources '("~/.authinfo.gpg")                 ; Auth Sources
 globals--browser      'browse-url-firefox                  ; Browser
 )

(setq user-full-name globals--name
	  user-mail-address globals--email
	  erc-nick globals--erc
	  erc-nick-short globals--irc
	  rcirc-default-user-name globals--irc
	  rcirc-default-nick      globals--irc
	  rcirc-default-full-name globals--name)

;;---------------------------------------------------------------------
;; BASE
;;---------------------------------------------------------------------

;; Load authinfo
(if (file-exists-p globals--auth-info)
	(setq auth-sources globals--auth-sources)
  (setq auth-source-cache-expiry nil))

;; Ask for encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Email to encrypt to
(setq epa-file-encrypt-to '(globals--email))

;; Garbage collection minibuffer hack
(defun my-minibuffer-setup-hook ()
  "Garbage collection will never occur."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Garbage collection will kick off immediately."
  (setq gc-cons-threshold gc-cons-threshold-original))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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

(setq bookmark-save-flag 1)
(setq bookmark-save-flag t)

(setq bookmark-set-fringe-mark nil)

;; Load any custom themes
(when (file-exists-p (expand-file-name "themes/" user-emacs-directory))
  (setq custom-safe-themes t)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory)))

;; Enable vc symlinks
(setq vc-follow-symlinks t)

;; Kill buffer settings
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(add-hook 'kill-buffer-query-functions
		  (lambda () (not-modified) t))

;; Backup settings
(setq-default backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq vc-make-backup-files nil)

;; Delete whitespace on save
(add-hook 'before-save-hook
		  'delete-trailing-whitespace)

;; Locale/Lang settings
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Default dirs
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

;; Scroll settings
(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil)
;; The most important setting of all! Make each scroll-event move 2 lines at
;; a time (instead of 5 at default). Simply hold down shift to move twice as
;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
;; mouse-wheel-scroll-amount '(2 ((shift) . 8) ((control) . 6)))

(setq scroll-margin 0)
;; (setq scroll-conservatively 100000)
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

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Menu-bar
(if (fboundp 'menu-bar-mode)
	(menu-bar-mode 0))

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

(defun update-scroll-bars ()
  (interactive)
  (mapc (lambda (win)
		  (set-window-scroll-bars win nil))
		(window-list))
  (set-window-scroll-bars (selected-window) 14 'right))

(defun enable-local-scroll-bar ()
  "Enable local buffer scroll bar."
  (interactive)
  (update-scroll-bars)
  (add-hook 'window-configuration-change-hook 'update-scroll-bars)
  (add-hook 'buffer-list-update-hook 'update-scroll-bars))

(defun disable-local-scroll-bar ()
  "Disable local buffer scroll bar."
  (interactive)
  (remove-hook 'window-configuration-change-hook 'update-scroll-bars)
  (remove-hook 'buffer-list-update-hook 'update-scroll-bars))

(defun local-scroll-bar-toggle ()
  "Toggle local buffer scroll bar."
  (interactive)
  (if (get 'lscroll-bar-toggle 'state)
	  (progn
		(disable-local-scroll-bar)
		(put 'lscroll-bar-toggle 'state nil))
	(progn
	  (enable-local-scroll-bar)
	  (put 'lscroll-bar-toggle 'state t))))

;; enable local scroll bar by default
;;(enable-local-scroll-bar)

;; tab bar
;;(tab-bar-enable)
;; (tab-bar-mode 1)
;; (setq tab-bar-show t)
;; (tab-bar-history-mode 1)
(setq tab-bar-history-limit 25)
;; (setq tab-bar-new-tab-choice "*GNU Emacs*")
;; (setq tab-bar-close-button-show nil)
;; (setq tab-bar-new-button-show nil)

;; tab bar when using emacsclient
(use-package emacs
  :hook (server-after-make-frame . tab-bar-enable))

;; time in tab-bar
;; (display-time-mode 1)
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)

;; (setq tab-bar-format '(tab-bar-format-global)
;; 	  tab-bar-mode t)

;; Configure fringe
(fringe-mode nil)
;;(fringe-mode 0)
;; (set-face-attribute 'fringe nil :background "#ffffff" :foreground "#ffffff")
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newline-into-fringe t)

;; Set default frame-size
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 46))

;; Set default frame background
;;(add-to-list 'default-frame-alist '(background-color . "honeydew"))

;; Reverse video mode
(defun reverse-video-mode ()
  "Reverse video mode."
  (interactive)
  (add-to-list 'default-frame-alist '(reverse . t)))
;;(reverse-video-mode)

;; ;; also reverse scroll-bar
;; (custom-set-faces
;;  '(scroll-bar
;;    ((t
;; 	 (:foreground "gray20" :background "black")))))

;; Enable some modes
(column-number-mode 1)

(global-hl-line-mode 0)

(global-prettify-symbols-mode t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
;;(add-hook 'md4rd-mode-hook 'visual-line-mode)

;; Aliases
(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'third 'caddr)
(defalias 'when-not 'unless)
(defalias 'word-count 'count-words)
(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'shrink-wrap 'fit-frame-to-buffer)

(defalias 'recentf-delete-list 'recentf-edit-list)

(defalias 'bookmark-delete-all 'bookmark-delete)

(defalias 'sync/news 'elfeed-update)

(defalias 'sync/work 'ejira-update-my-projects)

;; Set browser
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

;; Disable global highlight
(add-hook 'eshell-mode-hook
		  (lambda () (global-hl-line-mode 0)))

(defun eshell/clear-scrollback ()
  "Clear the scrollback content of the eshell window."
  (let ((inhibit-read-only t))
	(erase-buffer)))

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
	  (progn
		(split-window-sensibly (selected-window))
		(other-window 1)
		(eshell))
	(switch-to-buffer-other-window "*eshell*")))

;;---------------------------------------------------------------------
;; KEYBINDS
;;---------------------------------------------------------------------

(define-prefix-command 'z-map)
(global-set-key (kbd "C-1") 'z-map)

;;
;; PRIVATE
;;

;; general
(define-key z-map (kbd "a") 'org-agenda)
(define-key z-map (kbd "f") 'find-file-other-frame)
(define-key z-map (kbd "g") 'golden-ratio)
(define-key z-map (kbd "2") 'make-frame-command)
(define-key z-map (kbd "1") 'config/vscode-mode)
(define-key z-map (kbd "0") 'config/vscode-kill)
(define-key z-map (kbd "o") 'olivetti-mode)
(define-key z-map (kbd "r") 'recentf-delete-list)
(define-key z-map (kbd "m") 'magit-status)
(define-key z-map (kbd "w") 'eww)

;; os-specific
(if (eq system-type 'gnu/linux)
	(define-key z-map (kbd "v") 'vterm))
(if (eq system-type 'windows-nt)
	(define-key z-map (kbd "v") 'eshell))

;; modeline
(define-key z-map (kbd "B") 'display-battery-mode)
(define-key z-map (kbd "T") 'display-time-mode)

;; functions
(define-key z-map (kbd "*") 'quick-calc)
(define-key z-map (kbd "D") 'scratch-buffer)
(define-key z-map (kbd "O") 'org-redisplay-inline-images)
(define-key z-map (kbd "G") 'org-mark-ring-goto)
(define-key z-map (kbd "H") 'global-hl-line-mode)
(define-key z-map (kbd "s") 'ispell-word)
(define-key z-map (kbd "W") 'elfeed)
(define-key z-map (kbd "F") 'follow-mode)
(define-key z-map (kbd "U") 'undo-redo)
(define-key z-map (kbd "i") 'consult-imenu)
(define-key z-map (kbd "l") 'minimap-mode)
(define-key z-map (kbd "h") 'treemacs)
(define-key z-map (kbd "p") 'prettier-js)

;; quick
(define-key z-map (kbd "x") 'switch-to-buffer-other-frame)
(define-key z-map (kbd "k") 'compile)
(define-key z-map (kbd "e") 'eval-region)
(define-key z-map (kbd "b") 'browse-url)

;; auxiliary
(define-key z-map (kbd "S") 'speedbar-frame-mode)
(define-key z-map (kbd "2") 'consult-buffer-other-frame)
(define-key z-map (kbd "C-c") 'calendar)
(define-key z-map (kbd "C-d") 'dired-other-frame)

;; calendar
(define-key z-map (kbd ".") 'org-date-from-calendar)

;; files
(define-key z-map (kbd "n") 'notes-edit)
(define-key z-map (kbd "c") 'init-edit) ;; used to be config-edit when i was using config.org
(define-key z-map (kbd "I") 'inbox-edit)
(define-key z-map (kbd "t") 'tasks-edit)

;;
;; GLOBAL
;;

;; function
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'menu-bar-mode)
(global-set-key (kbd "<f7>") 'scroll-bar-mode)
(global-set-key (kbd "<f8>") 'tool-bar-mode)
(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "s-<f5>") 'config/toggle-theme)
(global-set-key (kbd "S-<f5>") 'neotree-toggle-project-dir)
;; (global-set-key (kbd "S-<f5>") 'open-treemacs)
(global-set-key (kbd "S-<f6>") 'config/toggle-fringe)
(global-set-key (kbd "S-<f7>") 'local-scroll-bar-toggle)
(global-set-key (kbd "S-<f8>") 'other-frame)
(global-set-key (kbd "<f9>") 'tab-bar-toggle)
(global-set-key (kbd "S-<f9>") 'toggle-frame-tab-bar)
(global-set-key (kbd "S-<f12>") 'display-line-numbers-mode)
(global-set-key (kbd "C-`") 'vterm-toggle)

;; windows
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "C-x W") 'elfeed-update)
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x x") 'window-swap-states)
(global-set-key (kbd "<s-C-return>") 'eshell-other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer-list-buffers)

;; windmove
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)

;; next/prev
;; (global-set-key (kbd "C-<tab>") 'next-buffer)
;; (global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'tab-next)
(define-key global-map (kbd "C-S-n") #'next-15-lines)
(define-key global-map (kbd "C-S-p") #'previous-15-lines)

;; mouse
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

;; indent/de-indent selection by one tab length
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;; kill word without copying it to your clipboard
(global-set-key (kbd "M-DEL") 'sanemacs/backward-kill-word)
(global-set-key (kbd "C-DEL") 'sanemacs/backward-kill-word)

;;---------------------------------------------------------------------
;; PACKAGES
;;---------------------------------------------------------------------

;; Core packages
(use-package fn        :demand t) ; function
(use-package s         :demand t) ; string
(use-package f         :demand t) ; file
(use-package ht        :demand t) ; hash table
(use-package dash      :demand t) ; lists
(use-package a         :demand t) ; assoc lists
(use-package ts        :demand t) ; timestamps
(use-package pcre2el   :demand t) ; sane regex
;;(use-package hierarchy :demand t) ; hierarchy
(use-package hierarchy
  :straight (:type built-in))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package async
  :straight t
  :demand t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

(use-package gcmh
  :straight t
  :init
  (setq gcmh-idle-delay 15
		gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))

(use-package org
  :straight (:type built-in)
  :diminish org-indent-mode
  :config
  ;; sensible defaults
  (setq ;; initial-major-mode 'org-mode
   org-display-inline-images t
   org-redisplay-inline-images t
   org-image-actual-width nil
   org-hide-emphasis-markers nil
   org-startup-with-inline-images "inlineimages"
   org-catch-invisible-edits 'smart
   org-id-track-globally t
   org-pretty-entities t)

  ;; indentation
  (setq org-src-preserve-indentation nil
		org-adapt-indentation nil)

  ;; set org directory & agenda files
  (when (file-directory-p "~/org")
	(setq org-directory "~/org"
		  org-agenda-files (list "~/org/inbox.org"
								 "~/org/tasks.org"
								 "~/org/notes.org"
								 "~/org/daily.org"
								 "~/org/work-notes.org")
		  org-default-notes-file "~/org/inbox.org"
		  org-id-locations-file "~/org/.orgids"))

  (setq org-agenda-custom-commands
		'(("n" "Non-work"
		   ;; Show all todos and everything due today, except work related tasks.
		   ((agenda "")
			(todo "TODO"))
		   ((org-agenda-files (remove "~/org/jira" org-agenda-files))))
		  ("N" "Agenda and all TODOs"
		   ((agenda #1="")
			(alltodo #1#)))))


  ;; set org todo keywords
  (setq org-todo-keywords
		'((sequence "TODO(t)"
					"WIP(w)"
					"WAITING"
					"|"
					"DONE(d)"
					"DEFERRED"
					"CANCELLED(c)")))

  ;; persist org-clock
  (setq org-clock-persist 'history)
  ;; (org-clock-persistence-insinuate)

  ;; set org-refile targets
  (when (file-directory-p "~/org")
	(setq org-refile-targets
		  '(("~/org/archive.org" :maxlevel . 1)
			("~/org/tasks.org" :maxlevel . 1))))

  ;; save org buffers after refiling!
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; org-src languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk         . t)
	 (calc        . t)
	 (css         . t)
	 (emacs-lisp  . t)
	 (gnuplot     . t)
	 (haskell     . t)
	 (js          . t)
	 (lisp        . t)
	 (org         . t)
	 (python      . t)
	 (ditaa       . t)
	 (scheme      . t)
	 (shell       . t)
	 (C           . t)
	 (sql         . t)))

  ;; org templates
  (setq org-capture-templates
		'(("i" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox")
		   "* %?\n%a\nEntered on %U" :empty-lines 1)
		  ("t" "Tasks" entry (file+olp+datetree "~/org/tasks.org" "Tasks")
		   "* %?\n%a\nEntered on %U" :empty-lines 1)))

  :bind
  ("C-c c" . 'org-capture)
  ("C-c a" . 'org-agenda)
  ("C-c l" . 'org-store-link)
  ("C-<f1>" . (lambda()(interactive)(show-all))))

(use-package org-contrib
  :straight t)

;; enable dnd images into org-mode & dired buffers
(when (file-directory-p "~/org/img")
  (use-package org-download
	:straight t
	:after org
	:config
	(setq-default org-download-image-dir "~/org/img/download")
	(add-hook 'dired-mode-hook 'org-download-enable)))

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package all-the-icons-dired
  :straight t
  :diminish all-the-icons-dired-mode
  :after all-the-icons
  :hook (dired-mode . (lambda ()
						(interactive)
						(unless (file-remote-p default-directory)
						  (all-the-icons-dired-mode)))))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "nomacs")
								("jpg" . "nomacs")
								("gif" . "nomacs")
								("mp4" . "mpv")
								("mkv" . "mpv")
								("webm" . "mpv")
								("pdf" . "zathura")
								("docx" . "libreoffice")
								("xlsx" . "libreoffice")
								("pptx" . "libreoffice"))))

(use-package dired-hide-dotfiles
  :straight t
  :diminish dired-hide-dotfiles-mode
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package evil
  :straight t
  :defer nil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  ;; enable evil-mode
  (evil-mode 1)

  ;; more granular undo with evil
  (setq evil-want-fine-undo t)

  ;; set evil state on a per mode basis
  ;; insert
  (evil-set-initial-state 'vterm-mode 'insert)
  ;; normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'mu4e-main-mode 'emacs)
  ;; motion
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  ;; emacs
  (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'Custom-mode 'emacs)
  ;; (evil-set-initial-state 'bufler-list-mode 'emacs)
  (evil-set-initial-state 'profiler-report-mode 'emacs)
  (evil-set-initial-state 'inferior-scheme-mode 'emacs)
  ;; (evil-set-initial-state 'md4rd-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'dictionary-mode ' emacs)

  ;; <tab> cycles org-mode visiblity
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  ;; :q kills buffer
  (evil-ex-define-cmd "q" 'delete-window))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) calc diff-mode))
  (evil-collection-init)
  ;; use dired-open
  (evil-collection-define-key 'normal 'dired-mode-map
	(kbd "RET") 'dired-find-alternate-file)
  (evil-collection-define-key 'normal 'dired-mode-map
	(kbd "S-<return>") 'dired-open-file))

(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
		sp-highlight-wrap-overlay nil
		sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
	(setq sp-show-pair-from-inside t)
	(setq sp-cancel-autoskip-on-backward-movement nil)
	(setq sp-pair-overlay-keymap (make-sparse-keymap)))

  (let ((unless-list '(sp-point-before-word-p
					   sp-point-after-word-p
					   sp-point-before-same-p)))
	(sp-pair "'"  nil :unless unless-list)
	(sp-pair "\"" nil :unless unless-list))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
				 "[" nil :post-handlers '(:rem ("| " "SPC")))


  (dolist (brace '("(" "{" "["))
	(sp-pair brace nil
			 :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
			 ;; Don't autopair opening braces if before a word character or
			 ;; other opening brace. The rationale: it interferes with manual
			 ;; balancing of braces, and is odd form to have s-exps with no
			 ;; whitespace in between, e.g. ()()(). Insert whitespace if
			 ;; genuinely want to start a new form in the middle of a word.
			 :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))

(use-package compile
  :straight t
  :init
  (setq compilation-ask-about-save nil
		compilation-scroll-output 'next-error
		;; Don't stop on info or warnings.
		compilation-skip-threshold 2))

;; Print elapsed time in compilation buffer
(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
		 (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
	(save-excursion (goto-char (point-max)) (insert msg))
	(message "Compilation %s: %s" (string-trim-right why) msg)))

(use-package vs-dark-theme
  :defer 3
  :straight t)

(use-package doom-themes
  :defer 3
  :straight t)
;; :config
;; (load-theme 'doom-dark+ t)
;; (custom-set-faces
;;  '(scroll-bar
;; 	 ((t
;; 	   (:foreground "gray30" :background "gray15"))))))

(use-package doom-modeline
  ;;:init (doom-modeline-mode)
  :defer 3
  :config
  (setq doom-modeline-height 35)
  (setq doom-modeline-buffer-file-name-style 'file-name
		doom-modeline-enable-word-count t
		doom-modeline-buffer-encoding nil
		doom-modeline-icon t ;; Enable/disable all icons
		doom-modeline-modal-icon nil ;; Icon for Evil mode
		doom-modeline-hud t ;; Replaces scroll-bar
		doom-modeline-major-mode-icon t
		doom-modeline-major-mode-color-icon t
		doom-modeline-bar-width 3))

(use-package dracula-theme
  :defer 3
  :straight t)

(use-package zenburn-theme
  :defer 3
  :straight t)
;; :config
;; (custom-set-faces
;;  '(scroll-bar
;; 	 ((t
;; 	   (:background "gray31")))))
;; (load-theme 'zenburn t))

(use-package spacemacs-theme
  :defer 3
  :straight t)

(use-package ef-themes
  :defer 3
  :straight t)

;; (use-package modus-themes
;;   :straight (:type built-in)
;;   ;; :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
;;   ;; :init
;;   ;; load the theme files before enabling a theme
;;   ;; (modus-themes-load-themes)
;;   :custom
;;   (modus-themes-italic-constructs t)
;;   (modus-themes-bold-constructs t)
;;   (modus-themes-region '(accented bg-only no-extend))
;;   (modus-themes-hl-line nil))
;; ;; :config
;; ;; (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
;; ;; (load-theme 'modus-operandi t)
;; ;; :bind ("S-<f5>" . modus-themes-toggle))

(use-package standard-themes
  :straight (:type git :host gitlab :repo "protesilaos/standard-themes" :branch "main")
  ;; :init
  ;; (load-theme 'standard-dark :no-confirm)
  :config
  (setq standard-themes-bold-constructs t
		standard-themes-italic-constructs t
		standard-themes-mixed-fonts t
		standard-themes-variable-pitch-ui t
		standard-themes-mode-line-accented nil
		;; Accepts a symbol value:
		standard-themes-fringes 'subtle
		;; The following accept lists of properties
		standard-themes-links '(neutral-underline)
		standard-themes-region '(no-extend neutral intense)
		standard-themes-prompts '(bold italic)))
;; (load-theme 'standard-dark t)
;; (custom-set-faces
;;  '(scroll-bar
;; 	 ((t
;; 	   (:foreground "dim gray" :background "black"))))))

(use-package dashboard
  :straight t
  :diminish dashboard-mode
  :defer nil
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :preface
  ;; (defconst home-page-dir "~/.emacs.d/img")
  ;; (defconst home-banners-dir (join-path home-page-dir "banners"))
  ;; (defconst home-friday-gifs-dir (join-path home-banners-dir "fridays"))
  ;; (defun home--friday? ()
  ;;   "Return if today is friday.  () -> bool."
  ;;   (equal "Fri" (format-time-string "%a")))
  ;; (defun home/choose-gif ()
  ;;   "Chooses randomly a gif path from `home-banners-dir'.
  ;;  If today is a friday, it gets from `home-friday-gifs-dir'.
  ;;  \()->string"
  ;;  (let* ((dir (if (home--friday?)
  ;;				  home-friday-gifs-dir
  ;;				home-banners-dir))
  ;;		 (gifs (directory-files dir t ".*\\.gif$"))
  ;;		 (index (mod (random) (length gifs))))
  ;;	(nth index gifs)))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 'official)
  ;; (setq dashboard-startup-banner (expand-file-name globals--banner-path user-emacs-directory))
  ;; (setq dashboard-startup-banner  (if (display-graphic-p)
  ;; 									  (home/choose-gif)
  ;; 									(join-path home-banners-dir "text-banner.txt")))
  ;; (setq dashboard-startup-banner (join-path home-banners-dir "text-banner.txt"))
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)

  ;; set dashboard navigator buttons
  (when (file-directory-p "~/org")
	(setq dashboard-navigator-buttons
		  `(;; line 1
			((,nil
			  "Notes"
			  "Open Notes"
			  (lambda (&rest _) (notes-edit))
			  'default)
			 (nil
			  "Scratch"
			  "Open Scratch Buffer"
			  (lambda (&rest _) (create-scratch-buffer))
			  'default)
			 (nil
			  "Tasks"
			  "Open Tasks"
			  (lambda (&rest _) (tasks-edit))
			  'default)
			 ;; (nil
			 ;;  "Agenda"
			 ;;  "Open Org-Agenda"
			 ;;  (lambda (&rest _) (org-agenda))
			 ;;  'default)
			 (nil
			  "Inbox"
			  "Open Inbox"
			  (lambda (&rest _) (inbox-edit))
			  'default)))))

  (setq tab-bar-new-tab-choice "*dashboard*")
  (define-key z-map (kbd "D") 'dashboard-refresh-buffer)

  ;; setup dashboard
  (dashboard-setup-startup-hook))

(defun init-edit ()
  "Edit initialization file."
  (interactive)
  (if (file-exists-p (concat user-emacs-directory "init.el"))
	  (find-file (concat user-emacs-directory "init.el"))))
(defun config-edit ()
  "Edit configuration file."
  (interactive)
  (if (file-exists-p (concat user-emacs-directory "config.org"))
	  (find-file (concat user-emacs-directory "config.org"))))
(when (file-directory-p "~/org")
  (defun notes-edit ()
	"Edit notes file."
	(interactive)
	(if (file-exists-p "~/org/notes.org")
		(find-file "~/org/notes.org")))
  (defun tasks-edit ()
	"Edit tasks file."
	(interactive)
	(if (file-exists-p "~/org/tasks.org")
		(find-file "~/org/tasks.org")))
  (defun archive-edit ()
	"Edit archive file."
	(interactive)
	(if (file-exists-p "~/org/archive.org")
		(find-file "~/org/archive.org")))
  (defun inbox-edit ()
	"Edit inbox file."
	(interactive)
	(if (file-exists-p "~/org/inbox.org")
		(find-file "~/org/inbox.org"))))
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(use-package diminish
  :straight t
  :init
  ;; diminish as mode is already loaded
  (diminish 'auto-revert-mode "")
  (diminish 'abbrev-mode "")
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'outline-mode)
  (diminish 'gcmh-mode)
  :config
  ;; diminish after mode is loaded
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "c-mode" '(diminish 'c-mode))
  (eval-after-load "c++-mode" '(diminish 'c++-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (eval-after-load "org" '(diminish 'org-indent-mode))
  (eval-after-load "ox-beamer" '(diminish 'org-beamer-mode))
  (eval-after-load "outline" '(diminish 'outline-minor-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "dired" '(diminish 'dired-async-mode))
  (eval-after-load "dired" '(diminish 'dired-hide-dotfiles-mode))
  (eval-after-load "dired" '(diminish 'all-the-icons-dired-mode))
  (eval-after-load "magit" '(diminish 'auto-fill-mode ""))
  (eval-after-load "magit" '(diminish 'with-editor-mode ""))
  (eval-after-load "slime" '(diminish 'slime-autodoc-mode ""))
  (eval-after-load "olivetti" '(diminish 'olivetti-mode ""))
  (eval-after-load "evil" '(diminish 'evil-collection-unimpaired-mode ""))
  (eval-after-load "mu4e" '(diminish 'overwrite-mode))
  (eval-after-load "mu4e" '(diminish 'mu4e-modeline-mode))
  (eval-after-load "auto-revert-mode" '(diminish 'auto-revert-mode "")))

(use-package golden-ratio
  :straight t
  :config
  ;;(setq golden-ratio-exclude-modes '("minimap-sb-mode" "minimap-mode" "neotree-mode" "treemacs-mode"))
  (setq golden-ratio-exclude-modes '("bs-mode"
									 "calc-mode"
									 "ediff-mode"
									 "dired-mode"
									 "gud-mode"
									 "gdb-locals-mode"
									 "gdb-registers-mode"
									 "gdb-breakpoints-mode"
									 "gdb-threads-mode"
									 "gdb-frames-mode"
									 "gdb-inferior-io-mode"
									 "gud-mode"
									 "gdb-inferior-io-mode"
									 "gdb-disassembly-mode"
									 "gdb-memory-mode"
									 "restclient-mode"
									 "speedbar-mode"
									 "minimap-sb-mode"
									 "minimap-mode")))

(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(straight-use-package
 '(info-variable-pitch
   :type git :host github
   :repo "kisaragi-hiu/info-variable-pitch"))

(add-hook 'Info-mode-hook #'info-variable-pitch-mode)

(use-package highlight-indent-guides
  :straight t
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'typescript-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (defun indent-guides-init-faces ()
	"Set indent-guides faces"
	(interactive)
	(set-face-background 'highlight-indent-guides-odd-face "gray30")
	(set-face-background 'highlight-indent-guides-even-face "gray30")
	(set-face-foreground 'highlight-indent-guides-character-face "gray30"))
  (defun vs-code-h-i-g ()
	"Set VSCode themed indent-guides faces"
	(interactive)
	(set-face-background 'highlight-indent-guides-odd-face "#404040")
	(set-face-background 'highlight-indent-guides-even-face "#404040")
	(set-face-foreground 'highlight-indent-guides-character-face "#404040"))
  (indent-guides-init-faces))

(use-package minimap
  :disabled t
  :diminish minimap-mode
  :preface
  (defun dark-minimap ()
	"Dark minimap."
	(interactive)
	(custom-set-faces
	 '(minimap-active-region-background
	   ((((background dark)) (:background "#121212121212")) ;;#15
		(t (:background "#121212121212"))) ;;#ce
	   "Face for the active region in the minimap.
  By default, this is only a different background color."
	   :group 'minimap)))
  (defun light-minimap ()
	"Light minimap."
	(interactive)
	(custom-set-faces
	 '(minimap-active-region-background
	   ((((background dark)) (:background "#151515151515"))
		(t (:background "#d9d9d9d9d9d9"))) ;;#ce
	   "Face for the active region in the minimap.
  By default, this is only a different background color."
	   :group 'minimap)))
  :config
  (setq minimap-window-location 'right)
  (setq minimap-width-fraction 0.05)
  (setq minimap-minimum-width 15)
  (setq minimap-hide-fringes t)
  (setq minimap-major-modes '(typescript-mode))
  ;;(setq minimap-update-delay 0)
  (light-minimap))

(use-package projectile
  :straight t
  ;; :bind (:map projectile-mode-map
  ;; 			  ("C-c p" . projectile-command-map))
  :init
  (setq projectile-sort-order 'recentf
		projectile-enable-caching t)
  ;; set projectile project path
  ;;(when (file-directory-p "~/src")
  ;;(setq projectile-project-search-path '("~/src" . 1)))
  :config
  (global-set-key (kbd "C-c p p") 'projectile-switch-project)
  (projectile-mode +1))

(use-package rg
  :straight t)

(use-package ag
  :straight t)

(use-package so-long
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

(use-package xah-math-input
  :straight
  ( :repo "diamondbond/xah-math-input"
	:host github
	:type git
	:files ("xah-math-input.el")))

(use-package flymake
  :straight t
  :config
  ;; Message navigation bindings
  (with-eval-after-load 'flymake
	(define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
	(define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))
  :hook ((emacs-lisp-mode) . flymake-mode))

(use-package flyspell
  :straight t
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  ;; (dolist (mode '(;; org-mode-hook ;; slows down org-mode
  ;; 				  mu4e-compose-mode-hook))
  ;; 	(add-hook mode (lambda () (flyspell-mode 1))))
  (if (eq system-type 'gnu/linux)
	  (setq ispell-program-name "hunspell"))
  (if (eq system-type 'windows-nt)
	  (setq ispell-program-name "aspell"))
  :bind (("M-<f7>" . flyspell-buffer)))

(use-package treemacs
  :disabled t
  :init
  (add-hook 'treemacs-mode-hook
			(lambda () (treemacs-resize-icons 15)))
  :config
  (progn
	(setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
		  treemacs-deferred-git-apply-delay      0.5
		  treemacs-display-in-side-window        t
		  treemacs-eldoc-display                 t
		  treemacs-file-event-delay              5000
		  treemacs-file-follow-delay             0.2
		  treemacs-follow-after-init             t
		  treemacs-git-command-pipe              ""
		  treemacs-goto-tag-strategy             'refetch-index
		  treemacs-indentation                   2
		  treemacs-indentation-string            " "
		  treemacs-is-never-other-window         nil
		  treemacs-max-git-entries               5000
		  treemacs-missing-project-action        'ask
		  treemacs-no-png-images                 nil
		  treemacs-no-delete-other-windows       t
		  treemacs-project-follow-cleanup        nil
		  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
		  treemacs-recenter-distance             0.1
		  treemacs-recenter-after-file-follow    nil
		  treemacs-recenter-after-tag-follow     nil
		  treemacs-recenter-after-project-jump   'always
		  treemacs-recenter-after-project-expand 'on-distance
		  treemacs-user-mode-line-format         'none
		  treemacs-show-cursor                   nil
		  treemacs-show-hidden-files             t
		  treemacs-silent-filewatch              nil
		  treemacs-silent-refresh                nil
		  treemacs-sorting                       'alphabetic-desc
		  treemacs-space-between-root-nodes      t
		  treemacs-tag-follow-cleanup            t
		  treemacs-tag-follow-delay              1.5
		  treemacs-width                         25)
	(treemacs-resize-icons 11)

	(treemacs-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode t)
	(pcase (cons (not (null (executable-find "git")))
				 (not (null (executable-find "python3"))))
	  (`(t . t)
	   (treemacs-git-mode 'deferred))
	  (`(t . _)
	   (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
		("M-0"       . treemacs-select-window)
		("C-x t 1"   . treemacs-delete-other-windows)
		("C-x t t"   . treemacs)
		("C-x t B"   . treemacs-bookmark)
		("C-x t C-t" . treemacs-find-file)
		("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :disabled t
  :after treemacs)

(use-package treemacs-evil
  :disabled treemacs evil
  :straight t)

(use-package neotree
  :straight t
  :after (projectile)
  :preface
  (defun neotree-show-file (&optional buff-name)
	"Show current file in neotree if it is open and there is a project open."
	(interactive)
	(let ((project-dir (projectile-project-root))
		  (file-name (or buff-name (buffer-file-name))))
	  (when (and project-dir (neo-global--window-exists-p))
		(neotree-dir project-dir)
		(neotree-find file-name))))
  (defun neotree-toggle-project-dir ()
	"Toggle NeoTree using the project root (if any) and find file."
	(interactive)
	(let ((curr-name (buffer-file-name)))
	  (neotree-toggle)
	  (neotree-show-file curr-name)))
  :custom
  ;;(neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-theme 'icons)
  (neo-window-position 'left)
  (neo-window-width 30)
  (neo-show-hidden-files t))

(use-package htmlize
  :straight t)

(if (eq system-type 'gnu/linux)
	(use-package vterm
	  :straight t
	  :custom (vterm-install t)
	  :config
	  (setq vterm-always-compile-module t)
	  (setq vterm-buffer-name-string "vterm %s")
	  (add-hook 'vterm-mode-hook
				(lambda () (global-hl-line-mode 0)))
	  (setq vterm-max-scrollback 10000)))

(if (eq system-type 'gnu/linux)
	(use-package multi-vterm
	  :straight t
	  :after vterm
	  :config
	  (add-hook 'vterm-mode-hook
				(lambda ()
				  ;;(setq-local evil-insert-state-cursor 'box)
				  (evil-insert-state)))))

(use-package vterm-toggle
  :straight t
  :config
  (setq vterm-toggle-fullscreen-p nil))
;; (add-to-list 'display-buffer-alist
;; 			 '((lambda (buffer-or-name _)
;; 				 (let ((buffer (get-buffer buffer-or-name)))
;; 				   (with-current-buffer buffer
;; 					 (or (equal major-mode 'vterm-mode)
;; 						 (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;; 			   ;;(display-buffer-reuse-window display-buffer-at-bottom)
;; 			   ;;(display-buffer-reuse-window display-buffer-in-direction)
;; 			   ;;(direction . bottom)
;; 			   ;;(dedicated . t) ;dedicated is supported in emacs27
;; 			   (reusable-frames . visible)
;; 			   (window-height . 0.3))))

(use-package crux
  :straight t)

(use-package 0x0
  :straight t
  :commands (0x0-dwim 0x0-popup 0x0-upload-file 0x0-upload-text))

(use-package command-log-mode
  :disabled t
  :diminish command-log-mode)

(use-package grip-mode
  :straight t
  :defer 1
  :bind (:map markdown-mode-command-map
			  ("g" . grip-mode)))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :config
  ;; Bind the `magit-status' command to a convenient key.
  (global-set-key (kbd "C-c g") #'magit-status)

  (defun parse-url (url)
	"convert a git remote location as a HTTP URL"
	(if (string-match "^http" url)
		url
	  (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
								"https://\\2/\\3"
								url)))
  (defun magit-open-repo ()
	"open remote repo URL"
	(interactive)
	(let ((url (magit-get "remote" "origin" "url")))
	  (progn
		(browse-url (parse-url url))
		(message "opening repo %s" url)))))

;; bindings to help improve the speed of magit
;; (use-package libgit :straight t)
;; (use-package magit-libgit :straight t)

(use-package autorevert
  :straight t
  :after magit
  :diminish auto-revert-mode
  :init
  (setq auto-revert-verbose nil)
  :hook ((prog-mode
		  text-mode
		  tex-mode
		  org-mode
		  conf-mode) . auto-revert-mode))

;; use dabbrev with Corfu!
(use-package dabbrev
  :straight t
  ;; swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
		 ("C-M-/" . dabbrev-expand)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region)))

(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  ;; (setq which-key-prefix-prefix "◉ ")
  (setq which-key-sort-order 'which-key-key-order-alpha
		which-key-min-display-lines 2
		which-key-max-display-columns 4))

(use-package switch-window
  :straight t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
		'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

;; a few more useful configurations...
(use-package emacs
  :init
  ;; add prompt indicator to `completing-read-multiple'.
  ;; alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
	(cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
		#'command-completion-default-include-p)

  ;; enable recursive minibuffers
  ;; (setq enable-recursive-minibuffers t)

  ;; completion ignores case
  (setq read-buffer-completion-ignore-case t
		read-file-name-completion-ignore-case t
		completion-ignore-case t)

  ;; allow Emacs to resize mini windows
  (setq resize-mini-windows t))

(use-package gtags
  :straight t
  :config
  (setq xref-prompt-for-identifier nil))

(use-package corfu
  :straight t
  :demand t
  :bind (:map corfu-map
			  ("<escape>". corfu-quit)
			  ("<return>" . corfu-insert)
			  ("C-h" . corfu-show-documentation)
			  ("M-l" . 'corfu-show-location)
			  ("RET" . nil)
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0)
  (corfu-echo-documentation 0)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s) ;; Necessary for use with orderless
  (corfu-scroll-margin 5) ;; Use scroll margin
  :init (global-corfu-mode)
  :config
  ;; adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
	"Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
	(unless (bound-and-true-p vertico--input)
	  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

(use-package cape
  :straight t
  ;; :bind (("C-c p p" . completion-at-point)
  ;; 		 ("C-c p d" . cape-dabbrev)
  ;; 		 ("C-c p f" . cape-file)
  ;; 		 ("C-c p s" . cape-symbol)
  ;; 		 ("C-c p i" . cape-ispell))
  :config
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
	(add-to-list 'completion-at-point-functions backend)))

;; enable vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
			  ("C-j" . vertico-next)
			  ("C-k" . vertico-previous)
			  ("M-j" . vertico-next)
			  ("M-k" . vertico-previous)
			  ("C-f" . vertico-exit)
			  :map minibuffer-local-map
			  ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode)
  :config
  (vertico-mouse-mode))

;; configure directory extension.
(use-package vertico-directory
  :straight nil
  :load-path "straight/repos/vertico/extensions"
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless partial-completion basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c j" . consult-line)
   ("C-c D" . consult-flymake)

   ;; ("C-c m" . consult-mode-command)
   ("C-c b" . consult-bookmark)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings (ctl-x-map)
   ("C-x b" . consult-buffer)
   ("C-x M-:" . consult-complex-command)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)

   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s bindings (search-map)
   ;; ("M-s f" . consult-find)
   ;; ("M-s L" . consult-locate)
   ;; ("M-s g" . consult-grep)
   ;; ("M-s G" . consult-git-grep)
   ;; ("M-s r" . consult-ripgrep)
   ;; ("M-s l" . consult-line)
   ;; ("M-s m" . consult-multi-occur)
   ;; ("M-s k" . consult-keep-lines)
   ;; ("M-s u" . consult-focus-lines)
   ("C-c f" . consult-ripgrep)

   ;; Custom bindings that map to ivy
   ("C-c r" . consult-recent-file)
   ;; ("C-c o" . consult-file-externally)
   ("C-s" . consult-line)

   ;; Other custom bindings
   ("M-y" . consult-yank-from-kill-ring)
   ("<help> a" . consult-apropos))

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)

  ;; Updating the default to include "--ignore-case"
  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --ignore-case --no-heading --line-number . -e ARG OPTS"))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none))))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons
  :straight t
  :config
  (cl-defmacro db/all-the-icons--with(&key name)
	(let ((defun-fn (intern (concat "jf/all-the-icons--with-" name)))
		  (icon-fn (intern (concat "all-the-icons-" name)))
		  (docstring (concat "Displays an ICON from `all-the-icons-" name "'.")))
	  `(defun ,defun-fn (icon str &optional height v-adjust)
		 ,docstring
		 (s-concat (,icon-fn
					icon
					:v-adjust (or v-adjust 0)
					:height (or height 1))
				   " " str))))
  (db/all-the-icons--with :name "faicon")
  (db/all-the-icons--with :name "material")
  (db/all-the-icons--with :name "octicon")
  (db/all-the-icons--with :name "alltheicon"))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  ;; Have background color be the same as `corfu' face background
  (kind-icon-default-face 'corfu-default)
  ;; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dictionary
  :straight t
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config (setq dictionary-server "dict.org"))

(use-package engine-mode
  :straight t
  :config
  (defengine google "https://google.com/search?q=%s" :keybinding "g"
			 :docstring "Search Google.")
  (defengine google-images "https://www.google.com/search?tbm=isch&q=%s" :keybinding "i"
			 :docstring "Search Google Images")
  ;; (defengine google-maps "http://maps.google.com/maps?q=%s" :keybinding "M"
  ;; 			 :docstring "Search Google Maps.")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d"
			 :docstring "Search DuckDuckGo.")
  (defengine qwant "https://www.qwant.com/?q=%s" :keybinding "q"
			 :docstring "Search Qwant.")
  (defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s" :keybinding "w"
			 :docstring "Search Wikipedia.")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y"
			 :docstring "Search YouTube.")
  (defengine twitter "https://twitter.com/search?q=%s" :keybinding "t"
			 :docstring "Search Twitter.")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "G"
			 :docstring "Search GitHub.")
  (defengine melpa "https://melpa.org/#/?q=%s" :keybinding "m"
			 :docstring "Search the Milkypostman's Emacs Lisp Package Archive.")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s"
			 :docstring "Search Stack Overflow.")
  (defengine wolfram-alpha "http://www.wolframalpha.com/input/?i=%s" :keybinding "a"
			 :docstring "Search Wolfram Alpha.")
  (defengine rfcs "http://pretty-rfc.herokuapp.com/search?q=%s" :keybinding "r"
			 :docstring "Search RFC documents.")
  (defengine ctan "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s" :keybinding "c"
			 :docstring "Search the Comprehensive TeX Archive Network")
  (defengine project-gutenberg "http://www.gutenberg.org/ebooks/search/?query=%s" :keybinding "p"
			 :docstring "Search Project Gutenberg.")
  (engine/set-keymap-prefix (kbd "C-x /"))
  (setq engine/browser-function globals--browser)
  :init
  (engine-mode t))

(use-package deadgrep
  :straight t
  :commands deadgrep)

(use-package avy
  :straight t
  :bind
  ("M-s" . avy-goto-char))

(when (file-directory-p "~/org")
  (use-package deft
	:straight t
	:config
	(setq deft-directory org-directory
		  deft-recursive t
		  deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
		  deft-use-filename-as-title t)
	:bind
	("C-c n d" . deft)))

(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
		'(("https://www.archlinux.org/feeds/news/" archlinux)
		  ("https://www.gnome.org/feed/" gnome)
		  ("http://nullprogram.com/feed/" nullprog)
		  ("https://planet.emacslife.com/atom.xml" emacs community)
		  ("https://www.ecb.europa.eu/rss/press.html" economics eu)
		  ("https://drewdevault.com/blog/index.xml" drew devault)
		  ("https://news.ycombinator.com/rss" ycombinator news)
		  ("https://www.phoronix.com/rss.php" phoronix))))
;;(define-key evil-normal-state-map (kbd "RET") 'elfeed-search-show-entry))

(use-package gnus
  :straight t
  :config
  ;; make Gnus startup faster
  (setq gnus-check-new-newsgroups nil
		gnus-check-bogus-newsgroups nil)

  ;; read feeds/atom through Gmane
  (setq gnus-select-method '(nntp "news.gmane.io"))

  ;; Gmail
  (setq gnus-select-method
		'(nnimap "gmail"
				 (nnimap-address "imap.gmail.com")))

  ;; make Gnus prettier
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► ")
  (setq gnus-summary-display-arrow t)
  (setq gnus-summary-line-format
		(concat
		 "%0{%U%R%z%}"
		 "%3{│%}" "%1{%d%}" "%3{│%}"
		 "  "
		 "%4{%-20,20f%}"
		 "  "
		 "%3{│%}"
		 " "
		 "%1{%B%}"
		 "%s\n"))

  ;; fixing summary buffer
  ;; there’s no need to recenter the summary buffer all the time, it only slows gnus down.
  (setq gnus-auto-center-summary nil)

  ;; enter the summary buffer faster
  (setq gnus-nov-is-evil nil
		gnus-show-threads t
		gnus-use-cross-reference nil)

  ;; news check
  (defun gnus-demon-scan-news ()
	(interactive)
	(when gnus-plugged
	  (let ((win (current-window-configuration))
			(gnus-read-active-file nil)
			(gnus-check-new-newsgroups nil)
			(gnus-verbose 2)
			(gnus-verbose-backends 5))
		(unwind-protect
			(save-window-excursion
			  (when (gnus-alive-p)
				(with-current-buffer gnus-group-buffer
				  (gnus-group-get-new-news gnus-activate-level))))
		  (set-window-configuration win)))))

  ;; configuring mail appearance
  (setq gnus-treat-strip-multiple-blank-lines t)
  (setq gnus-treat-trailing-blank-lines t)
  ;; let's see some smiles in gnus
  (setq gnus-treat-display-smileys t)
  (setq gnus-treat-emphasize 'head)

  ;; fetch only part of the article if we can.
  (setq gnus-read-active-file 'some)
  ;; fetch some old headers
  (setq gnus-fetch-old-headers 'some)

  ;; Gnus automatic scoring
  (setq gnus-use-adaptive-scoring t)

  ;; Gnus sorting
  (setq gnus-thread-sort-functions
		'(gnus-thread-sort-by-most-recent-date
		  (not gnus-thread-sort-by-number))))

(use-package erc
  :straight t
  :custom
  (erc-autojoin-timing 'ident)
  (erc-autojoin-channels-alist '(("irc.rizon.net" "#rice")))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-quit-reason 'erc-quit-reason-normal)
  (erc-timestamp-format "[%I:%M %p] ")
  (erc-timestamp-only-if-changed-flag nil)
  (erc-truncate-mode t)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
							 "324" "329" "332" "333" "353" "477"))
  :config
  ;; login
  (setq erc-nickserv-identify-mode 'autodetect)
  ;; interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)
  ;; kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
  ;; kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
  ;; kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)
  ;; open query buffers in the current window
  (setq erc-query-display 'buffer)
  ;; disable track keybinds messages
  (setq erc-track-enable-keybindings nil)
  ;; configure appearance
  (setq erc-prompt " >")
  ;; load erc modules
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling))

(use-package rcirc
  :defer
  :commands (irc rcirc)
  :ensure nil
  :config
  (setq rcirc-auto-authenticate-flag t)
  (setq rcirc-time-format "[%I:%M %p] ")

  ;; connect to Librea
  (setq rcirc-server-alist
		'(("irc.libera.chat" :channels ("#emacs")
		   :port 6697 :encryption tls)))

  ;; enable minor mode
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (rcirc-track-minor-mode 1))

(use-package nov
  :straight t
  :defer nil
  :config
  (defun nov-font-setup ()
	(face-remap-add-relative 'variable-pitch :family "Liberation Serif"
							 :height 1.0)
	(text-scale-increase 2))
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . nov-font-setup))

(use-package pdf-tools
  :straight t
  :defer nil
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; install pdf-tools
  (pdf-tools-install :no-query)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  (define-pdf-cache-function pagelabels)
  :hook ((pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
		 (pdf-view-mode-hook . pdf-tools-enable-minor-modes)))

(use-package pdf-view-restore
  :after pdf-tools
  :straight t
  :config
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package academic-phrases
  :straight t)

(use-package writegood-mode
  :straight t)

(use-package synosaurus
  :straight t)

(use-package olivetti
  :straight t
  :init
  (setq olivetti-body-width .75))

(use-package saveplace
  :straight t
  :defer nil
  :config
  (save-place-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package altcaps
  :straight (:type git :host github :repo "protesilaos/altcaps" :branch "main"))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
		  '(orderless))) ;; Configure orderless
  :hook ((c-mode          	; clangd
		  c++-mode        	; clangd
		  c-or-c++-mode   	; clangd
		  js2-mode        	; ts-ls (tsserver wrapper)
		  js-mode         	; ts-ls (tsserver wrapper)
		  rjsx-mode       	; ts-ls (tsserver wrapper)
		  js-jsx-mode     	; ts-ls (tsserver wrapper)
		  typescript-mode 	; ts-ls (tsserver wrapper)
		  python-mode     	; pyright
		  rust-mode       	; rust-analyzer
		  ruby-mode       	; solargraph
		  web-mode        	; ts-ls/HTML/CSS
		  clojure-mode		; clojure
		  clojurescript-mode	; clojurescript
		  clojurec-mode		; clojurec
		  ) . lsp-deferred)
  ((lsp-completion-mode . my/lsp-mode-setup-completion))
  ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-completion-at-point t)
  ;;(setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq completion-styles '(orderless)
		completion-category-defaults nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-clients-typescript-server "typescript-language-server"
		lsp-clients-typescript-server-args '("--stdio"))
  (setq lsp-disabled-clients '(eslint)))

(use-package lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-pyright
  :straight t
  :after lsp
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :init (setq lsp-pyright-python-executable-cmd "python3"))

;; (use-package yasnippet
;;   :straight t
;;   :diminish yas-minor-mode
;;   :config
;;   ;;(setq yas-snippet-dirs '("~/emacs.d/snippets/"))
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :defer 4
;;   :straight t)

;; (use-package auto-yasnippet
;;   :disabled t)

;; (use-package lsp-bridge
;;   :straight (:type git :host github :repo "manateelazycat/lsp-bridge" :branch "master")
;;   :files (:defaults ".py .tsx .js .cpp .c" "langserver" "acm")
;;   :init
;;   (global-lsp-bridge-mode))

;; Eglot
(use-package eglot
  :disabled t
  :config
  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  ;; Enable LSP support by default in programming buffers
  (add-hook 'prog-mode-hook #'eglot-ensure))

;; Pop-up auto-completion
(use-package company
  :disabled t
  :hook (prog-mode . company-mode))

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
		 (lambda () (require 'ccls) (lsp))))

(use-package modern-cpp-font-lock
  :straight t)

;; astyle formatter function
(defun astyle-buffer (&optional justify)
  "Format buffer using astyle --style=kr."
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
	(shell-command-on-region
	 (point-min)
	 (point-max)
	 "astyle --style=kr"
	 nil
	 t)
	(goto-line saved-line-number)))

(use-package csharp-mode
  :disabled t)

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (defun db/go-mode-hook()
	(setq tab-width 2)
	(add-hook 'before-save-hook 'gofmt-before-save)
	(set (make-local-variable 'compile-command)
		 "go test"))
  :hook ((go-mode . lsp-deferred))
  :hook ((go-mode . db/go-mode-hook))
  :hook ((go-mode . subword-mode)))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer t)

(use-package rustic
  :disabled t
  :config
  (setq rustic-format-on-save nil))

(use-package elisp-format
  :straight t)

(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-quicklisp)))

(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(mit guile))
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  (setq geiser-default-implementation 'mit)
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-to-list 'auto-mode-alist
			   '("\\.sls\\'" . scheme-mode)
			   '("\\.sc\\'" . scheme-mode)))

(use-package geiser-mit
  :straight t
  :after geiser)

(defun geiser-save ()
  "Save geiser repl contents to input ring."
  (interactive)
  (geiser-repl--write-input-ring))

(use-package sicp
  :straight t)

(use-package json-mode
  :straight t
  :mode ("\\.json\\'" . json-mode))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode))

(use-package lua-mode
  :straight t
  :config
  (setq lua-indent-level 2))

(use-package ruby-mode
  :straight t)

(use-package python-mode
  :straight t
  :config
  (setq python-indent-offset standard-indent)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "/usr/bin/python3")
  (setq exec-path (cons "~/.local/bin" exec-path)))

(use-package pyvenv
  :straight t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

(use-package python-black
  :straight t
  :after python)

(use-package xah-wolfram-mode
  :straight (:type git :host github :repo "xahlee/xah-wolfram-mode" :branch "master"))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :hook ((markdown-mode . auto-fill-mode)))

(use-package js2-mode
  :straight t
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package rjsx-mode
  :disabled t)

(use-package tree-sitter
  :straight t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :straight t
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
	"TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  (add-hook 'typescriptreact-mode-hook #'setup-tide-mode))

;; load ts-repl with ts-mode
;;(with-eval-after-load "typescript-mode"
;;(require 'ts-repl)))
;;(flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
;; :mode (("\\.ts\\'" . typescript-mode)
;; 		 ("\\.tsx\\'" . typescript-mode)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  ;;:quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; Add node_modules to PATH
(use-package add-node-modules-path
  :straight t
  :hook ((typescript-mode . add-node-modules-path)))

;; TypeScript IDE
(use-package tide
  :straight t
  :config
  (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package prettier-js
  :after (typescript-mode)
  :straight t)
;; :config
;; (add-hook 'web-mode-hook #'(lambda ()
;; 							   (enable-minor-mode
;; 								'("\\.jsx?\\'" . prettier-js-mode))
;; 							   (enable-minor-mode
;; 								'("\\.tsx?\\'" . prettier-js-mode)))))

(use-package web-mode
  :straight t
  :config
  (setq web-mode-content-types-alist '(;;("jsx" . "\\.[jt]sx?\\'")
									   ("html" . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)

  ;; adjust web-mode indent
  (defun web-mode-init-hook ()
	"Hooks for Web mode.  Adjust indent."
	(setq web-mode-markup-indent-offset 4))
  (add-hook 'web-mode-hook 'web-mode-init-hook)

  ;; disable other js linters
  (setq-default flycheck-disabled-checkers
				(append flycheck-disabled-checkers
						'(javascript-jshint json-jsonlist)))
  ;; enable eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :mode (("\\.js\\'" . web-mode)
		 ("\\.jsx\\'" .  web-mode)
		 ;;("\\.ts\\'" . web-mode)
		 ;;("\\.tsx\\'" . web-mode)
		 ("\\.html\\'" . web-mode))
  :commands web-mode)

(use-package lsp-java
  :disabled t
  :init
  (setq lsp-java-vmargs
		(list
		 "-noverify"
		 "-Xmx3G"
		 "-XX:+UseG1GC"
		 "-XX:+UseStringDeduplication"
		 )

		;; Don't organise imports on save
		lsp-java-save-action-organize-imports nil

		;; Fetch less results from the Eclipse server
		lsp-java-completion-max-results 30

		;; Download 3rd party sources from Maven repo
		lsp-java-maven-download-sources t

		;; Don't format my source code (I use Maven for enforcing my
		;; coding style)
		lsp-java-format-enabled nil)
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package dap-mode
  :disabled t
  ;;:after lsp-mode
  :config
  (dap-auto-configure-mode)
  (setq dap-ui-locals-expand-depth 3)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t
  :config
  (setq cider-repl-result-prefix "λ"
		cider-eval-result-prefix ""
		cider-connection-message-fn nil ; cute, but no!
		cider-use-overlays nil ; echo area is fine
		cider-repl-display-help-banner nil))

;;---------------------------------------------------------------------
;; FUNCTIONS
;;---------------------------------------------------------------------

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
  (tab-bar-history-mode 1))

(defun tab-bar-disable ()
  "Disable tab-bar."
  (interactive)
  (tab-bar-mode -1))

(defun tab-bar-toggle ()
  "Toggle tab-bar."
  (interactive)
  (if (get 'tab-bar-toggle 'state)
	  (progn
		(tab-bar-enable)
		(put 'tab-bar-toggle 'state nil))
	(progn
	  (tab-bar-disable)
	  (put 'tab-bar-toggle 'state t))))

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
  "Open marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (consult-file-externally (dired-get-filename))
   arg))

(defun open-treemacs ()
  (interactive)
  (treemacs)
  (other-window 1))

(defun desktop-restore ()
  (interactive)
  (desktop-read "~/Desktop"))

(if (eq system-type 'gnu/linux)
	(defun config/tangle ()
	  "Tangles this Emacs configuration."
	  (interactive)
	  (shell-command "~/.emacs.d/bin/tangle.sh")))

(if (eq system-type 'gnu/linux)
	(defun config/reload ()
	  "Reload this Emacs configuration."
	  (interactive)
	  (config/tangle)
	  (load-file (concat user-emacs-directory "init.el"))))

(when (file-directory-p "~/.emacs.d")
  (defun config/update ()
	"Updates this Emacs configuration."
	(interactive)
	(shell-command "cd ~/.emacs.d; git pull")
	(straight-pull-all)
	(config/reload)))

(defun config/github ()
  "Opens this configurations GitHub website."
  (interactive)
  (browse-url "https://github.com/diamondbond/emacs"))

(defun config/vscode-mode ()
  "Tree & Minimap."
  (interactive)
  ;;(disable-local-scroll-bar)
  ;;(tab-bar-enable)
  ;;(desktop-restore)
  ;;(config/vscode-theme)
  ;;(config/spacemacs-light-theme)
  (neotree-toggle-project-dir)
  (other-window 1))
;;(minimap-mode 1))
;;(indent-guides-init-faces))
;;(vs-code-h-i-g))
;; open git porcelain
;;(tab-switch "pos")
;;(other-window 1)
;;(vterm)
;;(magit)
;;(tab-switch "index.tsx"))

(defun config/vscode-kill ()
  "Kill treemacs & minimap."
  (interactive)
  ;;(disable-all-themes)
  ;;(light-minimap)
  ;;(scroll-bar/light)
  ;;(tab-bar-disable)
  ;;(doom-modeline-mode -1)
  ;; (treemacs)
  ;; (treemacs-kill-buffer)
  (neotree-hide))
;;(minimap-kill))

(defun config/light-theme ()
  "Light theme."
  (interactive)
  (disable-all-themes)
  (minimap-light)
  (init-indent-guide-faces)
  (scroll-bar/light))

(defun config/dark-theme ()
  "Dark theme."
  (interactive)
  (disable-all-themes)
  (scroll-bar/black)
  (minimap-dark)
  (vs-code-h-i-g)
  (load-theme 'standard-dark t))

(defun config/modus-theme-operandi ()
  "Light modus theme."
  (interactive)
  (disable-all-themes)
  (scroll-bar/light)
  (minimap-light)
  (init-indent-guide-faces)
  (load-theme 'modus-operandi t))

(defun config/modus-theme-vivendi ()
  "Dark modus theme."
  (interactive)
  (disable-all-themes)
  (scroll-bar/dark)
  (minimap-dark)
  (load-theme 'modus-vivendi t))

(defun config/toggle-theme ()
  "Toggle standard theme."
  (interactive)
  (if (get 'theme-toggle 'state)
	  (progn
		(config/light-theme)
		(put 'theme-toggle 'state nil))
	(progn
	  (config/dark-theme)
	  (put 'theme-toggle 'state t))))

(defun config/spacemacs-light-theme ()
  "Spacemacs light theme."
  (interactive)
  (disable-all-themes)
  (light-minimap)
  (scroll-bar/light)
  (doom-modeline-mode)
  (load-theme 'spacemacs-light t))

(defun config/spacemacs-dark-theme ()
  "Spacemacs dark theme."
  (interactive)
  (disable-all-themes)
  (dark-minimap)
  (scroll-bar/dark)
  (doom-modeline-mode)
  (load-theme 'spacemacs-dark t))

(defun config/vscode-theme ()
  "VSCode theme."
  (interactive)
  (disable-all-themes)
  (dark-minimap)
  (scroll-bar/vscode)
  (doom-modeline-mode)
  (load-theme 'doom-dark+ t))

(defun config/zenburn-theme ()
  "Zenburn theme."
  (interactive)
  (disable-all-themes)
  (scroll-bar/zenburn)
  (load-theme 'zenburn t))

(defun config/dracula-theme ()
  "Dracula theme."
  (interactive)
  (disable-all-themes)
  ;;(indent-guides-init-faces)
  (scroll-bar/dracula)
  (load-theme 'dracula t))

(defun scroll-bar/light ()
  "Set scroll-bar to light color."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "#7E8182":background "#cecece"))))))

(defun scroll-bar/dark ()
  "Set scroll-bar to dark color."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "dim gray" :background "gray20"))))))

(defun scroll-bar/zenburn ()
  "Set scroll-bar to zenburn color."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "gray" :background "gray31"))))))

(defun scroll-bar/black ()
  "Set scroll-bar to black color."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "dim gray" :background "black"))))))

(defun scroll-bar/dracula ()
  "Set scroll-bar to dracula color."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "#282A36" :background "#44475A"))))))

(defun scroll-bar/vscode ()
  "VSCode themed scrollbar."
  (interactive)
  (custom-set-faces
   '(scroll-bar
	 ((t
	   (:foreground "gray30" :background "gray15"))))))

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

;; (when (file-readable-p "~/bin/sync-dotfiles.sh")
;;   (defun sync/dotfiles ()
;; 	"Sync dotfiles."
;; 	(interactive)
;; 	(async-shell-command "~/bin/sync-dotfiles.sh")))

;; (when (file-readable-p "~/bin/sync-neocities.sh")
;;   (defun sync/neocities ()
;; 	"Sync neocities."
;; 	(interactive)
;; 	(save-window-excursion
;; 	  (async-shell-command "~/bin/sync-neocities.sh"))))

;; (when (file-readable-p "~/bin/sync-blog.sh")
;;   (defun sync/blog ()
;; 	"Sync blog."
;; 	(interactive)
;; 	(save-window-excursion
;; 	  (async-shell-command "~/bin/sync-blog.sh"))))

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

(if (eq system-type 'gnu/linux)
	(defun music ()
	  "Play music with ncmpcpp."
	  (interactive)
	  (run-in-vterm "ncmpcpp")))

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

(defun start-to-org-agenda ()
  "Launch shrink-wrapped 'org-agenda'."
  (interactive)
  (org-agenda nil "n")
  (delete-other-windows)
  (fit-frame-to-buffer))

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

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
	(if (use-region-p)
		(progn
		  (setq $p1 (region-beginning))
		  (setq $p2 (region-end)))
	  (save-excursion
		(if (re-search-backward "\n[ \t]*\n" nil "move")
			(progn (re-search-forward "\n[ \t]*\n")
				   (setq $p1 (point)))
		  (setq $p1 (point)))
		(re-search-forward "\n[ \t]*\n" nil "move")
		(skip-chars-backward " \t\n" )
		(setq $p2 (point))))
	(save-excursion
	  (save-restriction
		(narrow-to-region $p1 $p2)
		(goto-char (point-min))
		(while (re-search-forward " +" nil t)
		  (replace-match "\n" ))))))

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

;;---------------------------------------------------------------------
;; MODULES
;;---------------------------------------------------------------------

;; Load email module
;; (when (file-directory-p
;; 	   (concat user-emacs-directory "/modules"))
;;   (org-babel-load-file
;;    (concat user-emacs-directory "/modules/mail.org")))

(defun mu-setup/build-mu-binary ()
  "Compiles 'mu' binary."
  (interactive)
  (async-shell-command "cd ~/.emacs.d/straight/repos/mu; ./autogen.sh; ninja -C build"))

(defun mu-setup/init-mu ()
  "Initialize 'mu' db."
  (interactive)
  (async-shell-command "mu init --maildir=/home/diamond/mail/ --my-address=diamondbond1@gmail.com"))

(defun mu-setup/rebuild-mu-index ()
  "Rebuilds 'mu' index."
  (interactive)
  (async-shell-command "mu index"))

(defun mu-setup/automagic ()
  "Auto-magically configures 'mu'."
  (interactive)
  ;; (mu-setup/build-mu-binary)
  ;; (sit-for 5)
  (mu-setup/init-mu)
  ;; (sit-for 5)
  (mu-setup/rebuild-mu-index))

(defun sync/mail ()
  "Sync email."
  (interactive)
  (async-shell-command "offlineimap")
  (mu4e-update-index))

(use-package mu4e
  :straight t
  ;; :straight ( :host github
  ;; 			  :repo "djcb/mu"
  ;; 			  :branch "master"
  ;; 			  :files ("build/mu4e/*"))
  ;; :pre-build (("./autogen.sh") ("make")))
  :custom (mu4e-mu-binary "/usr/local/bin/mu")
  :diminish mu4e-headers-mode
  :diminish mu4e-modeline-mode
  :config
  ;; default
  ;; (require 'org-mu4e)
  (setq mu4e-maildir (expand-file-name "~/mail"))

  ;; set folders
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")
  ;; (setq mu4e-mu-home "~/.cache/mu")

  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
		'(("/INBOX"             . ?i)
		  ("/[Gmail].Sent Mail" . ?s)
		  ("/[Gmail].Trash"     . ?t)))

  ;; attachments go here
  (setq mu4e-attachment-dir  "~/mail/attachments")

  ;; modify behavior when putting something in the trash (T flag) so as
  ;; to make it sync to the remote server. This code deals with the bug
  ;; that, whenever a message is marked with the trash label T,
  ;; offlineimap wont sync it back to the gmail servers.
  ;;
  ;; NOTE: Taken from
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
	(if (zerop nth) (cdr list)
	  (let ((last (nthcdr (1- nth) list)))
		(setcdr last (cddr last))
		list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
			   '(trash
				 :char ("d" . "▼")
				 :prompt "dtrash"
				 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
				 :action (lambda (docid msg target)
						   (mu4e~proc-move docid
										   (mu4e~mark-check-target target) "-N"))))

  ;; inbox-query
  (setq db/mu4e-inbox-query
		"(maildir:/Inbox OR maildir:/INBOX) AND flag:unread")

  ;; go-to-inbox function
  (defun db/go-to-inbox ()
	"View unread inbox."
	(interactive)
	(mu4e-headers-search db/mu4e-inbox-query))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; why would I want to leave my message open after I've sent it?
  (setq message-kill-buffer-on-exit t)
  ;; don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; don't ask to quit
  (setq mu4e-confirm-quit nil)

  ;; define z-map keybind
  (define-key z-map (kbd "M") 'mu4e)

  ;; disable mu4e-modeline
  (mu4e-modeline-mode -1)
  (add-hook 'mu4e-main-mode-hook (lambda () (mu4e-modeline-mode -1)))
  ;;(add-hook 'dashboard-mode-hook (lambda () (mu4e-modeline-mode -1)))
  (add-hook 'emacs-startup-hook (lambda () (mu4e-modeline-mode -1)))

  ;; start mu4e
  (mu4e t))

(use-package mu4e-alert
  :disabled t
  :init
  (defun db/mu4e-notif ()
	"Display both mode line and desktop alerts for incoming new emails."
	(interactive)
	(mu4e-update-mail-and-index 1)        ; getting new emails is ran in the background
	(mu4e-alert-enable-mode-line-display) ; display new emails in mode-line
	(mu4e-alert-enable-notifications))    ; enable desktop notifications for new emails
  (defun db/mu4e-refresh ()
	"Refresh emails every 300 seconds and display desktop alerts."
	(interactive)
	(mu4e t)                            ; start silently mu4e (mandatory for mu>=1.3.8)
	(run-with-timer 0 300 'db/mu4e-notif))
  :after mu4e
  :bind ("<f2>" . db/mu4e-refresh)  ; F2 turns Emacs into a mail client
  :config

  ;; show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query db/mu4e-inbox-query)

  ;; show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  ;; mode line alerts:
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

  ;; desktop alerts
  (mu4e-alert-set-default-style 'libnotify)
  ;; auto-enable notifications when opening mu4e
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  ;; enable notifications
  (mu4e-alert-enable-notifications))

(use-package smtpmail
  :straight t
  :config
  (setq message-send-mail-function 'smtpmail-send-it
		starttls-use-gnutls t
		smtpmail-starttls-credentials
		'(("smtp.gmail.com" 587 nil nil))
		smtpmail-auth-credentials
		(expand-file-name "~/.authinfo.gpg")
		smtpmail-default-smtp-server "smtp.gmail.com"
		smtpmail-smtp-server "smtp.gmail.com"
		smtpmail-smtp-service 587
		smtpmail-debug-info t))

;; Load jira module
;; (when (file-directory-p
;; 	   (concat user-emacs-directory "/modules"))
;;   (org-babel-load-file
;;    (concat user-emacs-directory "/modules/jira.org")))

(use-package ejira
  :straight (:type git :host github :repo "nyyManni/ejira" :branch "master")
  :defer 2
  :init
  (setq jiralib2-url              "https://sallypos.atlassian.net"
		jiralib2-auth             'token
		jiralib2-user-login-name  "diamondbond1@gmail.com"

		;; NOTE, this directory needs to be in `org-agenda-files'`
		ejira-org-directory       "~/org/jira"
		ejira-projects            '("SP")

		ejira-priorities-alist    '(("Highest" . ?A)
									("High"    . ?B)
									("Medium"  . ?C)
									("Low"     . ?D)
									("Lowest"  . ?E))
		ejira-todo-states-alist   '(("To Do"       . 1)
									("In Progress" . 2)
									("CODE REVIEW" . 3)
									("Done"        . 4)))
  ;; Load jira token
  (when (file-readable-p "~/org/jira/jiralib2-token.el")
	(load-file "~/org/jira/jiralib2-token.el"))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
	 ((ejira-jql "resolution = unresolved and assignee = currentUser()"
				 ((org-agenda-overriding-header "Assigned to me")))))))

(use-package jira-markup-mode
  :straight t
  :defer 4)

;; Start server
;;(server-start)

;; Restore original GC values
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold gc-cons-threshold-original)
			(setq gc-cons-percentage gc-cons-percentage-original)))

;;; init.el ends here
