;;; -*- lexical-binding: t; -*-

;;---------------------------------------------------------------------
;; CORE
;;---------------------------------------------------------------------

;; Clean up the modeline
(use-package diminish
  :init
  ;; diminish as mode is already loaded
  (diminish 'auto-revert-mode "")
  (diminish 'abbrev-mode "")
  (diminish 'visual-line-mode)
  (diminish 'outline-mode)
  ;; diminish after mode is loaded
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (eval-after-load "ox-beamer" '(diminish 'org-beamer-mode))
  (eval-after-load "outline" '(diminish 'outline-minor-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  (eval-after-load "dired" '(diminish 'dired-async-mode))
  (eval-after-load "magit" '(diminish 'auto-fill-mode ""))
  (eval-after-load "magit" '(diminish 'with-editor-mode ""))
  (eval-after-load "olivetti" '(diminish 'olivetti-mode ""))
  (eval-after-load "evil" '(diminish 'evil-collection-unimpaired-mode "")))

;; Inherit path from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))
  (when (daemonp)
	(exec-path-from-shell-initialize)))

;; Direnv
(use-package direnv
  :config
  (direnv-mode))

;; Garbage Collection Magic Hack
(use-package gcmh
  :diminish gcmh-mode
  :hook (emacs-startup . gcmh-mode)
  :custom (gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

;; Async
(use-package async
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

;; Save place in buffer
(use-package saveplace
  :straight (:type built-in)
  :init
  (save-place-mode))

;; Save history
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; Display keybinds
(use-package which-key
  :straight (:type built-in)
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 2)
  (which-key-max-display-columns 4))

;; Replace fancy major modes with basic major mode
;; when long lines are detected.
(use-package so-long
  :straight (:type built-in)
  :init (global-so-long-mode 1)
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward)))

;;---------------------------------------------------------------------
;; EVIL
;;---------------------------------------------------------------------

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t)
  (evil-cross-lines t)
  (evil-symbol-word-search t)
  :config
  ;; enable evil-mode
  (evil-mode 1)

  ;; evil-redo
  (define-key evil-normal-state-map (kbd "U") 'undo-redo)

  ;; set evil state on a per mode basis
  ;; insert
  (evil-set-initial-state 'vterm-mode 'insert)
  ;; normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; motion
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  ;; emacs
  (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'Custom-mode 'emacs)
  (evil-set-initial-state 'profiler-report-mode 'emacs)
  (evil-set-initial-state 'inferior-scheme-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'dictionary-mode ' emacs)

  ;; M-. jumps to definition (xref/eglot) instead of evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)

  ;; <tab> cycles org-mode visiblity
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  ;; :q kills buffer
  (evil-ex-define-cmd "q" 'delete-window))

;; EVIL extras
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) magit calc diff-mode))
  :config
  (evil-collection-init)
  ;; use dired-open
  (evil-collection-define-key 'normal 'dired-mode-map
	(kbd "RET") 'dired-find-alternate-file)
  (evil-collection-define-key 'normal 'dired-mode-map
	(kbd "S-<return>") 'dired-open-file)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
	(kbd "RET") 'elfeed-search-show-entry)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
	(kbd "S-<return>") 'elfeed-show-visit
	(kbd "SPC") 'scroll-up-command
	(kbd "S-SPC") 'scroll-down-command
	(kbd "C-j") 'elfeed-show-next
	(kbd "C-k") 'elfeed-show-prev
	"go" 'elfeed-show-visit
	"gr" 'elfeed-show-refresh
	"q" 'elfeed-kill-buffer))

;;---------------------------------------------------------------------
;; EDITING
;;---------------------------------------------------------------------

;; Use dabbrev with Corfu!
(use-package dabbrev
  :straight (:type built-in)
  ;; swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
		 ("C-M-/" . dabbrev-expand)))

;; Expand/highlight regions
(use-package expand-region
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region)))

;; Quick jump to any char on screen
(use-package avy
  :bind
  ("M-s" . avy-goto-char))

;; Input unicode symbols
(use-package xah-math-input
  :straight
  ( :repo "diamondbond/xah-math-input"
	:host github
	:type git
	:files ("xah-math-input.el")))

;; Spellchecking
(use-package flyspell
  :straight (:type built-in)
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (if (eq system-type 'gnu/linux)
	  (setq ispell-program-name "hunspell"))
  (if (eq system-type 'windows-nt)
	  (setq ispell-program-name "aspell"))
  :bind (("M-<f7>" . flyspell-buffer)))

;; Diagnostics for elisp
(use-package flymake
  :straight (:type built-in)
  :hook (emacs-lisp-mode . flymake-mode)
  :bind (:map flymake-mode-map
			  ("C-c n" . flymake-goto-next-error)
			  ("C-c p" . flymake-goto-prev-error)))

;; Configure Emacs compile command
(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'next-error)
  (compilation-skip-threshold 2))  ; don't stop on info or warnings

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

;;---------------------------------------------------------------------
;; COMPLETION
;;---------------------------------------------------------------------

;; Completion pre-configuration
(use-package emacs
  :custom
  ;; do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; hide commands in M-x which do not work in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; completion ignores case
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  ;; allow Emacs to resize mini windows
  (resize-mini-windows t)
  :hook (minibuffer-setup . cursor-intangible-mode))

;; Vertico minibuffer completions
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
			  ("C-j" . vertico-next)
			  ("C-k" . vertico-previous)
			  ("M-j" . vertico-next)
			  ("M-k" . vertico-previous)
			  ("C-f" . vertico-exit)
			  :map minibuffer-local-map
			  ("M-h" . backward-kill-word)
			  ("M-l" . kill-word))
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode)
  :config
  (vertico-mouse-mode))

;; Vertico directory extension
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word)))

;; Completion style for matching regexps in any order
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion annotations
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Consulting completing-read
(use-package consult
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c j" . consult-line)
   ("C-c D" . consult-flymake)
   ("C-c b" . consult-bookmark)
   ("C-c k" . consult-kmacro)
   ("C-c f" . consult-ripgrep)
   ("C-c r" . consult-recent-file)

   ;; C-x bindings (ctl-x-map)
   ("C-x b" . consult-buffer)
   ("C-x M-:" . consult-complex-command)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)

   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; Other bindings
   ("C-s" . consult-line)
   ("M-y" . consult-yank-from-kill-ring))
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; Act on minibuffer completions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  ;; key help through completing-read (overrides which-key, so load after it)
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

;; Embark & Consult integration
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu completion framework
(use-package corfu
  :demand t
  :bind (:map corfu-map
			  ("<escape>". corfu-quit)
			  ("<return>" . corfu-insert)
			  ("C-h" . corfu-info-documentation)
			  ("M-l" . corfu-info-location)
			  ("RET" . nil)
			  ("M-j" . corfu-next)
			  ("M-k" . corfu-previous)
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-scroll-margin 5)
  :init (global-corfu-mode)
  :config
  ;; adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
	"Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
	(unless (bound-and-true-p vertico--input)
	  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;; Completion at point extensions
(use-package cape
  :bind (("M-i" . completion-at-point)
		 ("C-c C-c d" . cape-dabbrev)
		 ("C-c C-c f" . cape-file)
		 ("C-c C-c s" . cape-symbol)
		 ("C-c C-c i" . cape-ispell))
  :custom (cape-dabbrev-min-length 3)
  :config
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
	(add-to-list 'completion-at-point-functions backend)))

;;---------------------------------------------------------------------
;; FILES
;;---------------------------------------------------------------------

;; Dired
(use-package dired
  :straight (:type built-in)
  :bind ("C-x C-j" . dired-jump)
  :custom (dired-listing-switches "-agho --group-directories-first")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; Open files externally from dired
(use-package dired-open
  :custom
  (dired-open-extensions '(("png" . "nomacs")
						   ("jpg" . "nomacs")
						   ("gif" . "nomacs")
						   ("mp4" . "mpv")
						   ("mkv" . "mpv")
						   ("webm" . "mpv")
						   ("pdf" . "zathura")
						   ("docx" . "libreoffice")
						   ("xlsx" . "libreoffice")
						   ("pptx" . "libreoffice"))))

;; Hide dotfiles in dired
(use-package dired-hide-dotfiles
  :diminish dired-hide-dotfiles-mode
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; Project management
(use-package project
  :straight (:type built-in)
  :bind (("C-c p p" . project-switch-project)
		 ("C-c p b" . consult-project-buffer)
		 ("C-c p s" . consult-ripgrep)
		 ("C-c p !" . project-shell-command)))

;; Treemacs
(use-package treemacs
  :init
  (add-hook 'treemacs-mode-hook
			(lambda () (treemacs-resize-icons 15)))
  :custom
  (treemacs-collapse-dirs (if (executable-find "python3") 3 0))
  (treemacs-eldoc-display t)
  (treemacs-file-event-delay 5000)
  (treemacs-follow-after-init t)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-user-mode-line-format 'none)
  (treemacs-width-is-initially-locked nil)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil)

;;---------------------------------------------------------------------
;; GIT
;;---------------------------------------------------------------------

;; Magit - the best git porcelain
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom (magit-diff-refine-hunk 'all)
  :config
  (evil-define-key 'normal magit-mode-map (kbd "K") #'magit-discard)
  (evil-define-key 'normal magit-mode-map (kbd "C-<tab>") #'tab-next)
  (evil-define-key 'normal magit-mode-map (kbd "C-<iso-lefttab>") #'tab-prev)

  (defun parse-url (url)
	"convert a git remote location as a HTTP URL"
	(if (string-match "^http" url)
		url
	  (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
								"https://\\2/\\3"
								url)))
  (defun open-github ()
	"open remote repo URL"
	(interactive)
	(let ((url (magit-get "remote" "origin" "url")))
	  (progn
		(browse-url (parse-url url))
		(message "opening repo %s" url)))))

;; Git diff in the fringe, refreshed after magit operations
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
		 (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom (diff-hl-draw-borders nil)
  :init
  (global-diff-hl-mode)
  :config
  (define-fringe-bitmap 'db/diff-hl-bar [224] nil nil '(center repeated))
  (define-fringe-bitmap 'db/diff-hl-delete [128 192 224 240] nil nil 'bottom)
  (setq diff-hl-fringe-bmp-function
		(lambda (type _pos)
		  (if (eq type 'delete) 'db/diff-hl-delete 'db/diff-hl-bar)))
  (diff-hl-flydiff-mode))

;;---------------------------------------------------------------------
;; TERMINAL
;;---------------------------------------------------------------------

;; VTerm
(if (eq system-type 'gnu/linux)
	(use-package vterm
	  :custom
	  ;; Must be set before load, or the daemon blocks on a y-or-n-p
	  (vterm-always-compile-module t)
	  (vterm-buffer-name-string "vterm %s")
	  (vterm-max-scrollback 10000)
	  :config
	  (add-hook 'vterm-mode-hook
				(lambda () (global-hl-line-mode 0)))))

;; Mutliplex vterm
(if (eq system-type 'gnu/linux)
	(use-package multi-vterm
	  :after vterm))

;; Toggle vterm
(use-package vterm-toggle)

;;---------------------------------------------------------------------
;; UI
;;---------------------------------------------------------------------

;; Themes
(use-package vscode-dark-plus-theme)

;; Doom themes
(use-package doom-themes
  :defer 3)

;; Modus themes (integrated into Emacs28+)
;; The 2024 gitlab checkout errors on Emacs 31 (face inheritance cycle).
(use-package modus-themes
  :straight (:type built-in)
  :no-require t
  :init
  (require-theme 'modus-themes)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides '((bg-region bg-magenta-intense)
										   (fg-region fg-main))))

;; Prot's EF themes
(use-package ef-themes
  :defer 3)

;; Dashboard
(use-package dashboard
  :diminish dashboard-mode
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (tab-bar-new-tab-choice "*dashboard*")
  (dashboard-items '((recents  . 5)
					 (projects . 5)))
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-startupify-list '(dashboard-insert-banner
							   dashboard-insert-newline
							   dashboard-insert-banner-title
							   dashboard-insert-newline
							   dashboard-insert-navigator
							   dashboard-insert-items))
  :config
  ;; set dashboard navigator buttons
  (when (file-directory-p "~/org")
	(setq dashboard-navigator-buttons
		  `(;; line 1
			((,nil
			  "Notes"
			  "Open Notes"
			  (lambda (&rest _) (notes-edit))
			  default)
			 (nil
			  "Scratch"
			  "Open Scratch Buffer"
			  (lambda (&rest _) (create-scratch-buffer))
			  default)
			 (nil
			  "Tasks"
			  "Open Tasks"
			  (lambda (&rest _) (tasks-edit))
			  default)
			 (nil
			  "Inbox"
			  "Open Inbox"
			  (lambda (&rest _) (inbox-edit))
			  default)))))

  ;; setup dashboard
  (dashboard-setup-startup-hook))

;; Dashboard related functions
(defun init-edit ()
  "Edit initialization file."
  (interactive)
  (if (file-exists-p (concat user-emacs-directory "init.el"))
	  (find-file (concat user-emacs-directory "init.el"))))
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
  "Open the persistent scratch file `globals--scratch'."
  (interactive)
  (find-file globals--scratch))

;; Switch windows quickly
(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

;; Colorize color-codes in prog-modes
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Highlight matching brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Focus writing mode
(use-package olivetti
  :custom (olivetti-body-width .75))

;; Fancy icons (glyphs come from any installed Nerd Font)
(use-package nerd-icons
  :custom (nerd-icons-font-family "SauceCodePro Nerd Font Mono"))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda ()
						(unless (file-remote-p default-directory)
						  (nerd-icons-dired-mode)))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;;---------------------------------------------------------------------
;; ORG
;;---------------------------------------------------------------------

;; Org-mode - one mode to rule them all.
(use-package org
  :straight (:type built-in)
  :diminish org-indent-mode
  :custom
  (org-image-actual-width nil)
  (org-startup-with-link-previews t)
  (org-pretty-entities t)
  (org-clock-persist 'history)
  (org-todo-keywords '((sequence "TODO(t)"
								 "WIP(w)"
								 "WAITING"
								 "|"
								 "DONE(d)"
								 "DEFERRED"
								 "CANCELLED(c)")))
  (org-agenda-custom-commands
   '(("n" "Non-work"
	  ;; Show all todos and everything due today, except work related tasks.
	  ((agenda "")
	   (todo "TODO"))
	  ((org-agenda-files (remove "~/org/jira" org-agenda-files))))
	 ("N" "Agenda and all TODOs"
	  ((agenda "")
	   (alltodo "")))))
  (org-capture-templates
   '(("i" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox")
	  "* %?\n%a\nEntered on %U" :empty-lines 1)
	 ("t" "Tasks" entry (file+olp+datetree "~/org/tasks.org" "Tasks")
	  "* %?\n%a\nEntered on %U" :empty-lines 1)))
  :config
  ;; agenda, notes and refile targets
  (when (file-directory-p "~/org")
	(setq org-agenda-files (list "~/org/inbox.org"
								 "~/org/tasks.org"
								 "~/org/notes.org"
								 "~/org/daily.org"
								 "~/org/work-notes.org")
		  org-default-notes-file "~/org/inbox.org"
		  org-id-locations-file "~/org/.orgids"
		  org-refile-targets '(("~/org/archive.org" :maxlevel . 1)
							   ("~/org/tasks.org" :maxlevel . 1))))

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

  :bind
  ("C-c c" . 'org-capture)
  ("C-c a" . 'org-agenda)
  ("C-c l" . 'org-store-link)
  ("C-<f1>" . org-fold-show-all))

;; Org-mode extras
(use-package org-contrib)

(use-package org-present
  :defer 5)

;; Enable dnd images into org-mode & dired buffers
(when (file-directory-p "~/org/img")
  (use-package org-download
	:after org
	:custom (org-download-image-dir "~/org/img/download")
	:config
	(add-hook 'dired-mode-hook 'org-download-enable)))

;; Convert buffer text and decorations to HTML
(use-package htmlize)

;; Deft - search ~/org quickly
(when (file-directory-p "~/org")
  (use-package deft
	:defer 3
	:custom
	(deft-directory "~/org")
	(deft-recursive t)
	(deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
	(deft-use-filename-as-title t)))

;;---------------------------------------------------------------------
;; APPS
;;---------------------------------------------------------------------

;; Search engines, under C-x /
(use-package engine-mode
  :init
  (engine-mode t)
  :config
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine google-images "https://www.google.com/search?tbm=isch&q=%s" :keybinding "i")
  (defengine google-maps "https://maps.google.com/maps?q=%s" :keybinding "M")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d")
  (defengine qwant "https://www.qwant.com/?q=%s" :keybinding "q")
  (defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s" :keybinding "w")
  (defengine youtube "https://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "G")
  (defengine melpa "https://melpa.org/#/?q=%s" :keybinding "m")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s")
  (defengine wolfram-alpha "https://www.wolframalpha.com/input/?i=%s" :keybinding "a")
  (defengine ctan "https://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s" :keybinding "c")
  (defengine project-gutenberg "https://www.gutenberg.org/ebooks/search/?query=%s" :keybinding "p"))

;; Dictionary
(use-package dictionary
  :straight (:type built-in)
  :bind ("C-c d" . dictionary-search)
  :custom (dictionary-server "dict.org"))

;; RSS
(use-package elfeed
  :defer 2
  :custom
  (elfeed-feeds
   '(("https://www.archlinux.org/feeds/news/" archlinux)
	 ("https://www.gnome.org/feed/" gnome)
	 ("http://nullprogram.com/feed/" nullprog)
	 ("https://planet.emacslife.com/atom.xml" emacs community)
	 ("https://www.ecb.europa.eu/rss/press.html" economics eu)
	 ("https://drewdevault.com/blog/index.xml" drew devault)
	 ("https://news.ycombinator.com/rss" ycombinator news)
	 ("https://www.phoronix.com/rss.php" phoronix))))

;; Mailing lists
(use-package gnus
  :straight (:type built-in)
  :defer 2
  :custom
  ;; make Gnus startup faster
  (gnus-check-new-newsgroups nil)

  ;; make Gnus prettier
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-false-root "◯ ")
  (gnus-sum-thread-tree-single-indent "◎ ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► ")
  (gnus-summary-display-arrow t)
  (gnus-summary-line-format
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

  ;; there’s no need to recenter the summary buffer all the time, it only slows gnus down.
  (gnus-auto-center-summary nil)

  ;; enter the summary buffer faster
  (gnus-use-cross-reference nil)

  ;; configuring mail appearance
  (gnus-treat-strip-multiple-blank-lines t)
  (gnus-treat-strip-trailing-blank-lines t)
  (gnus-treat-display-smileys t)
  (gnus-treat-emphasize 'head)

  ;; fetch some old headers
  (gnus-fetch-old-headers 'some)

  ;; Gnus automatic scoring
  (gnus-use-adaptive-scoring t)

  ;; Gnus sorting
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date
	 (not gnus-thread-sort-by-number)))
  :config
  ;; Gmail (setq: as :custom it would load gnus at startup)
  (setq gnus-select-method '(nnimap "gmail"
									(nnimap-address "imap.gmail.com")))

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
		  (set-window-configuration win))))))

;; Rizon IRC client
(use-package erc
  :straight (:type built-in)
  :defer 2
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
  (erc-timestamp-format "[%I:%M %p] ")
  (erc-timestamp-only-if-changed-flag nil)
  (erc-truncate-mode t)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
							 "324" "329" "332" "333" "353" "477"))
  ;; login
  (erc-nickserv-identify-mode 'autodetect)
  ;; interpret mIRC-style color commands in IRC chats
  (erc-interpret-mirc-color t)
  ;; kill buffers for channels after /part
  (erc-kill-buffer-on-part t)
  ;; kill buffers for private queries after quitting the server
  (erc-kill-queries-on-quit t)
  ;; kill buffers for server messages after quitting the server
  (erc-kill-server-buffer-on-quit t)
  ;; open query buffers in the current window
  (erc-interactive-display 'buffer)
  ;; disable track keybinds messages
  (erc-track-enable-keybindings nil)
  ;; configure appearance
  (erc-prompt " >")
  :config
  ;; load erc modules
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling))

;; Librea IRC client
(use-package rcirc
  :straight (:type built-in)
  :defer 2
  :commands (irc rcirc)
  :custom
  (rcirc-time-format "[%I:%M %p] ")
  ;; connect to Librea
  (rcirc-server-alist
   '(("irc.libera.chat" :channels ("#emacs")
	  :port 6697 :encryption tls)))
  :config
  ;; enable minor mode
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (rcirc-track-minor-mode 1))

;; Epub reader
(use-package nov
  :config
  (defun nov-font-setup ()
	(face-remap-add-relative 'variable-pitch :family "Liberation Serif"
							 :height 1.0)
	(text-scale-increase 2))
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . nov-font-setup))

;; PDF reader
(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  ;; open pdfs scaled to fit page
  (pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (pdf-annot-activate-created-annotations t)
  :config
  ;; install pdf-tools
  (pdf-tools-install :no-query)
  (define-pdf-cache-function pagelabels))

;; Restore pdf view
(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode))

(provide 'packages)
;;; packages.el ends here
