;;; -*- lexical-binding: t; -*-
;; Inherit path from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))
  (when (daemonp)
	(exec-path-from-shell-initialize)))

;; Async
(use-package async
  :straight t
  :demand t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

;; Garbage Collection Magic Hack
(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 15
		gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

;; Org-mode - one mode to rule them all.
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

  ;; set org-refile targets
  (when (file-directory-p "~/org")
	(setq org-refile-targets
		  '(("~/org/archive.org" :maxlevel . 1)
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
  ("C-<f1>" . org-fold-show-all))

;; Org-mode extras
(use-package org-contrib
  :straight t)

(use-package org-present
  :straight t
  :defer 5)

;; Enable dnd images into org-mode & dired buffers
(when (file-directory-p "~/org/img")
  (use-package org-download
	:straight t
	:after org
	:config
	(setq-default org-download-image-dir "~/org/img/download")
	(add-hook 'dired-mode-hook 'org-download-enable)))

;; Dired
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

;; Direnv
(use-package direnv
  :straight t
  :config
  (direnv-mode))

;; EVIL
(use-package evil
  :straight t
  :defer nil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-symbol-word-search t)
  :config
  ;; enable evil-mode
  (evil-mode 1)

  ;; more granular undo with evil
  (setq evil-want-fine-undo t)

  ;; evil-redo
  (define-key evil-normal-state-map (kbd "U") 'undo-redo)

  ;; horizontal movement crosses lines
  (setq-default evil-cross-lines t)

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

  ;; <tab> cycles org-mode visiblity
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  ;; :q kills buffer
  (evil-ex-define-cmd "q" 'delete-window))

;; EVIL extras
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) magit calc diff-mode))
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

;; Smart parentheses
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

;; Configure Emacs compile command
(use-package compile
  :straight (:type built-in)
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

;;---------------------------------------------------------------------
;; THEMES
;;---------------------------------------------------------------------

(use-package vscode-dark-plus-theme
  :straight t)

;; Doom themes
(use-package doom-themes
  :straight t
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
  (modus-themes-common-palette-overrides
		'((bg-region bg-magenta-intense)
		  (fg-region fg-main))))

;; Prot's EF themes
(use-package ef-themes
  :defer 3
  :straight t)

;;---------------------------------------------------------------------
;; DASHBOARD
;;---------------------------------------------------------------------

(use-package dashboard
  :straight t
  :diminish dashboard-mode
  :defer nil
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents  . 5)
						  (projects . 5)))
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info nil) ;; hide init info
  (setq dashboard-set-footer nil) ;; hide footer
  (setq dashboard-set-navigator t) ;; show nav

  (setq dashboard-startupify-list '(dashboard-insert-banner
									dashboard-insert-newline
									dashboard-insert-banner-title
									dashboard-insert-newline
									dashboard-insert-navigator
									dashboard-insert-items))

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

  (setq tab-bar-new-tab-choice "*dashboard*")

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
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;; Clean up the modeline
(use-package diminish
  :straight t
  :init
  ;; diminish as mode is already loaded
  (diminish 'auto-revert-mode "")
  (diminish 'abbrev-mode "")
  (diminish 'visual-line-mode)
  (diminish 'outline-mode)
  ;; diminish after mode is loaded
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (eval-after-load "org" '(diminish 'org-indent-mode))
  (eval-after-load "ox-beamer" '(diminish 'org-beamer-mode))
  (eval-after-load "outline" '(diminish 'outline-minor-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "rust-mode" '(diminish 'lsp-lens-mode))
  (eval-after-load "typescript-ts-mode" '(diminish 'subword-mode))
  (eval-after-load "js2-mode" '(diminish 'subword-mode ""))
  (eval-after-load "dired" '(diminish 'dired-async-mode))
  (eval-after-load "dired" '(diminish 'dired-hide-dotfiles-mode))
  (eval-after-load "dired" '(diminish 'all-the-icons-dired-mode))
  (eval-after-load "magit" '(diminish 'auto-fill-mode ""))
  (eval-after-load "magit" '(diminish 'with-editor-mode ""))
  (eval-after-load "olivetti" '(diminish 'olivetti-mode ""))
  (eval-after-load "evil" '(diminish 'evil-collection-unimpaired-mode "")))

;; Colorize color-codes in prog-modes
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Highlight matching brackets
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Project management
(use-package projectile
  :straight t
  :init
  (setq projectile-sort-order 'recentf
		projectile-enable-caching t)
  :config
  (global-set-key (kbd "C-c p p") 'projectile-switch-project)
  (global-set-key (kbd "C-c p b") 'consult-project-buffer)
  (global-set-key (kbd "C-c p s") 'consult-ripgrep)
  (global-set-key (kbd "C-c p !") 'projectile-run-shell-command-in-root)
  (projectile-mode +1))

;; Replace fancy major modes with basic major mode
;; when long lines are detected.
(use-package so-long
  :straight (:type built-in)
  :init (global-so-long-mode 1)
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward)))

;; Input unicode symbols
(use-package xah-math-input
  :straight
  ( :repo "diamondbond/xah-math-input"
	:host github
	:type git
	:files ("xah-math-input.el")))

;; Diagnostics for elisp
(use-package flymake
  :straight (:type built-in)
  :config
  ;; Message navigation bindings
  (with-eval-after-load 'flymake
	(define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
	(define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))
  :hook ((emacs-lisp-mode) . flymake-mode))

;; Spellchecking
(use-package flyspell
  :straight (:type built-in)
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (if (eq system-type 'gnu/linux)
	  (setq ispell-program-name "hunspell"))
  (if (eq system-type 'windows-nt)
	  (setq ispell-program-name "aspell"))
  :bind (("M-<f7>" . flyspell-buffer)))

;; Treemacs
(use-package treemacs
  :straight t
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
		  treemacs-width-is-initially-locked     nil)

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

(use-package treemacs-all-the-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-projectile
  :straight t
  :after treemacs)

(use-package treemacs-evil
  :straight t)

;; Convert buffer text and decorations to HTML
(use-package htmlize
  :straight t)

;; VTerm
(if (eq system-type 'gnu/linux)
	(use-package vterm
	  :straight t
	  ;; Must be set before load, or the daemon blocks on a y-or-n-p
	  :custom (vterm-always-compile-module t)
	  :config
	  (setq vterm-buffer-name-string "vterm %s")
	  (add-hook 'vterm-mode-hook
				(lambda () (global-hl-line-mode 0)))
	  (setq vterm-max-scrollback 10000)))

;; Mutliplex vterm
(if (eq system-type 'gnu/linux)
	(use-package multi-vterm
	  :straight t
	  :after vterm
	  :config
	  (add-hook 'vterm-mode-hook
				(lambda ()
				  (evil-insert-state)))))

;; Toggle vterm
(use-package vterm-toggle
  :straight t
  :config
  (setq vterm-toggle-fullscreen-p nil))

;; Magit - the best git porcelain
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
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

;; Git diff
(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :diminish (git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git diff in the fringe
(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Use dabbrev with Corfu!
(use-package dabbrev
  :straight (:type built-in)
  ;; swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
		 ("C-M-/" . dabbrev-expand)))

;; Expand/highlight regions
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region)))

;; Display keybinds
(use-package which-key
  :straight (:type built-in)
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-sort-order 'which-key-key-order-alpha
		which-key-min-display-lines 2
		which-key-max-display-columns 4))

;; Switch windows quickly
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

;; Completion pre-configuration
(use-package emacs
  :init
  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
		#'command-completion-default-include-p)

  ;; completion ignores case
  (setq read-buffer-completion-ignore-case t
		read-file-name-completion-ignore-case t
		completion-ignore-case t)

  ;; allow Emacs to resize mini windows
  (setq resize-mini-windows t))

;; Corfu completion framework
(use-package corfu
  :straight t
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
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.2)
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

;; Completion at point extensions
(use-package cape
  :straight t
  :bind (("M-i" . completion-at-point)
		 ("C-c C-c d" . cape-dabbrev)
		 ("C-c C-c f" . cape-file)
		 ("C-c C-c s" . cape-symbol)
		 ("C-c C-c i" . cape-ispell))
  :config
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
	(add-to-list 'completion-at-point-functions backend)))

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
  :load-path "straight/repos/vertico/extensions"
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word)))

;; Completion style for matching regexps in any order
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless) ;; partial-completion basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion annotations
(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode))

;; Consulting completing-read
(use-package consult
  :straight t
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c j" . consult-line)
   ("C-c D" . consult-flymake)

   ("C-c b" . consult-bookmark)
   ("C-c k" . consult-kmacro)

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

   ("C-c f" . consult-ripgrep)

   ;; Custom bindings that map to ivy
   ("C-c r" . consult-recent-file)
   ("C-s" . consult-line)

   ;; Other custom bindings
   ("M-y" . consult-yank-from-kill-ring)
   ("<help> a" . consult-apropos))

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

;; Act on minibuffer completions
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
				 (window-parameters (mode-line-format . none)))))

;; Emark & Consult integrations
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Fancy icons
(use-package all-the-icons
  :straight t)

;; Fancy icons for completions
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Corfu completion icons
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

;; Dictionary
(use-package dictionary
  :straight (:type built-in)
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config (setq dictionary-server "dict.org"))

;; Search engine
(use-package engine-mode
  :straight t
  :config
  (defengine google "https://google.com/search?q=%s" :keybinding "g"
			 :docstring "Search Google.")
  (defengine google-images "https://www.google.com/search?tbm=isch&q=%s" :keybinding "i"
			 :docstring "Search Google Images")
  (defengine google-maps "http://maps.google.com/maps?q=%s" :keybinding "M"
			 :docstring "Search Google Maps.")
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

;; Quick jump to any char on screen
(use-package avy
  :straight t
  :bind
  ("M-s" . avy-goto-char))

;; Deft - search ~/org quickly
(when (file-directory-p "~/org")
  (use-package deft
	:straight t
	:defer 3
	:config
	(setq deft-directory org-directory
		  deft-recursive t
		  deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
		  deft-use-filename-as-title t)))

;; RSS
(use-package elfeed
  :straight t
  :defer 2
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

;; Mailing lists
(use-package gnus
  :straight (:type built-in)
  :defer 2
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

;; Librea IRC client
(use-package rcirc
  :straight (:type built-in)
  :defer 2
  :commands (irc rcirc)
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

;; Epub reader
(use-package nov
  :straight t
  :config
  (defun nov-font-setup ()
	(face-remap-add-relative 'variable-pitch :family "Liberation Serif"
							 :height 1.0)
	(text-scale-increase 2))
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . nov-font-setup))

;; PDF reader
(use-package pdf-tools
  :straight t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; install pdf-tools
  (pdf-tools-install :no-query)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  (define-pdf-cache-function pagelabels))

;; Restore pdf view
(use-package pdf-view-restore
  :after pdf-tools
  :straight t
  :hook (pdf-view-mode . pdf-view-restore-mode))

;; Focus writing mode
(use-package olivetti
  :straight t
  :init
  (setq olivetti-body-width .75))

;; Save place in buffer
(use-package saveplace
  :straight (:type built-in)
  :defer nil
  :config
  (save-place-mode))

;; Save history
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

(provide 'packages)
;;; packages.el ends here
