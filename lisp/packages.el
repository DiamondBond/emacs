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
  :init
  (setq gcmh-idle-delay 15
		gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config
  (setq gc-cons-threshold lsp-cons-threshold)
  (gcmh-mode 1))

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
  :config
  ;; enable evil-mode
  (evil-mode 1)

  ;; more granular undo with evil
  (setq evil-want-fine-undo t)

  ;; evil-redo
  ;; (setq evil-undo-system "undo-redo")
  (define-key evil-normal-state-map (kbd "U") 'undo-redo)

  ;; horizontal movement crosses lines
  (setq-default evil-cross-lines t)

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

;; EVIL extras
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) magit neotree calc diff-mode))
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

;; Docsets
(use-package dash-docs
  :straight t
  :defer 5)

;;---------------------------------------------------------------------
;; THEMES
;;---------------------------------------------------------------------

(use-package vs-dark-theme
  :disabled t
  :defer 3)

(use-package vscode-dark-plus-theme
  :straight t)
;; :defer 1)

;; Doom themes
(use-package doom-themes
  :straight t
  :defer 3)

;; Doom modeline
(use-package doom-modeline
  :straight t
  ;; :defer 1
  ;;:init (doom-modeline-mode)
  :config
  (setq doom-modeline-height 28)
  (setq doom-modeline-buffer-file-name-style 'file-name
		doom-modeline-enable-word-count t
		doom-modeline-buffer-encoding nil
		doom-modeline-buffer-modification-icon nil ;; Save icon
		doom-modeline-icon t ;; Enable/disable all icons
		doom-modeline-modal-icon nil ;; Icon for Evil mode
		doom-modeline-hud t ;; Replaces scroll-bar
		doom-modeline-major-mode-icon t
		doom-modeline-major-mode-color-icon t
		;; doom-modeline-time nil
		;; doom-modeline-battery nil
		doom-modeline-bar-width 3))

;; Zenburn theme
(use-package zenburn-theme
  :straight t
  :defer 3)

;; Sanityinc themes
(use-package color-theme-sanityinc-tomorrow
  :straight t
  :defer 3)
;; :config
;; (load-theme 'sanityinc-tomorrow-bright t))

;; Spacemacs themes
(use-package spacemacs-theme
  :straight t
  :defer 3)

;; Dracula theme
(use-package dracula-theme
  :disabled t
  :defer 3)

;; Modus themes (integrated into Emacs28+)
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

;; Prot's EF themes
(use-package ef-themes
  :defer 3
  :straight t)

;; Enhanced default themes
(use-package standard-themes
  :disabled t
  ;;:straight (:type git :host gitlab :repo "protesilaos/standard-themes" :branch "main")
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
  :preface
  ;; Random banner
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
  (setq dashboard-items '((recents  . 5)
						  (projects . 5)
						  (bookmarks . 5)))
  ;;(agenda . 2)))
  (setq dashboard-banner-logo-title nil)
  ;; (setq dashboard-startup-banner 'official)
  (setq dashboard-startup-banner (expand-file-name globals--banner-path user-emacs-directory))
  ;; (setq dashboard-startup-banner  (if (display-graphic-p)
  ;; 									  (home/choose-gif)
  ;; 									(join-path home-banners-dir "text-banner.txt")))
  (setq dashboard-center-content t)
  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info nil) ;; hide init info
  (setq dashboard-set-footer nil) ;; hide footer
  (setq dashboard-set-navigator t) ;; show nav

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

  ;; setup dashboard
  (dashboard-setup-startup-hook))

;; Dashboard related functions
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

;; Centaur Tabs
(use-package centaur-tabs
  :straight t
  :preface
  (defun enable-centaur-tabs ()
	(interactive)
	(put 'centaur-tabs-toggle 'state t)
	(centaur-tabs-mode -1)
	(centaur-tabs-mode)
	(centaur-tabs-headline-match))
  (defun disable-centaur-tabs ()
	(interactive)
	(put 'centaur-tabs-toggle 'state nil)
	(centaur-tabs-mode -1))
  (defun toggle-centaur-tabs ()
	(interactive)
	(if (get 'centaur-tabs-toggle 'state)
		(disable-centaur-tabs)
	  (enable-centaur-tabs)))
  :hook
  ;;(dashboard-mode . centaur-tabs-local-mode)
  ;;(term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-group-by-projectile-project) ;;for https://github.com/ema2159/centaur-tabs/issues/181#issuecomment-1075806796
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar"
		centaur-tabs-height 22
		centaur-tabs-set-icons nil
		centaur-tabs-plain-icons nil
		;; centaur-tabs-gray-out-icons #'buffer
		centaur-tabs-set-bar #'under
		x-underline-at-descent-line t
		;; centaur-tabs-close-button "×"
		;; centaur-tabs-modified-marker "•"
		;; centaur-tabs-show-new-tab-button t
		;; centaur-tabs-show-count t
		centaur-tabs-show-navigation-buttons t)
  (setq centaur-tabs-set-close-button nil)
  (defun centaur-tabs-buffer-groups ()
	"`centaur-tabs-buffer-groups' control buffers' group rules.

  Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
  All buffer name start with * will group to \"Emacs\".
  Other buffer group by `centaur-tabs-get-group-name' with project name."
	(list
	 (cond
	  ((or (string-equal "*" (substring (buffer-name) 0 1))
		   (memq major-mode '(magit-process-mode
							  magit-status-mode
							  magit-diff-mode
							  magit-log-mode
							  magit-file-mode
							  magit-blob-mode
							  magit-blame-mode
							  )))
	   "Emacs")
	  ((derived-mode-p 'prog-mode)
	   "Editing")
	  ((derived-mode-p 'dired-mode)
	   "Dired")
	  ((memq major-mode '(vterm-mode
						  term-mode))
	   "Term")
	  ((memq major-mode '(erc-mode
						  rcirc-mode))
	   "IRC")
	  ((memq major-mode '(helpful-mode
						  help-mode))
	   "Help")
	  ((memq major-mode '(org-mode
						  org-agenda-clockreport-mode
						  org-src-mode
						  org-agenda-mode
						  org-beamer-mode
						  org-indent-mode
						  org-bullets-mode
						  org-cdlatex-mode
						  org-agenda-log-mode
						  diary-mode))
	   "OrgMode")
	  (t
	   (centaur-tabs-get-group-name (current-buffer))))))
  (defun centaur-tabs-hide-tab (x)
	"Do no to show buffer X in tabs."
	(let ((name (format "%s" x)))
	  (or
	   ;; Current window is not dedicated window.
	   (window-dedicated-p (selected-window))

	   ;; Buffer name not match below blacklist.
	   (string-prefix-p "*epc" name)
	   (string-prefix-p "*helm" name)
	   (string-prefix-p "*Helm" name)
	   (string-prefix-p "*Compile-Log*" name)
	   (string-prefix-p "*Semantic SymRef*" name)
	   (string-prefix-p "*copilot events*" name)
	   (string-prefix-p "*lsp" name)
	   (string-prefix-p "*company" name)
	   (string-prefix-p "*Flycheck" name)
	   (string-prefix-p "*Flymake" name)
	   (string-prefix-p "*tramp" name)
	   (string-prefix-p " *Mini" name)
	   (string-prefix-p "*help" name)
	   (string-prefix-p "*straight" name)
	   (string-prefix-p " *temp" name)
	   (string-prefix-p "*Help" name)

	   (string-prefix-p "*direnv*" name)
	   (string-prefix-p "*scratch*" name)
	   (string-prefix-p "*Messages" name)

	   ;; cpp
	   (string-prefix-p "*clangd" name)
	   (string-prefix-p "*clangd::stderr" name)
	   (string-prefix-p "*ccls*" name)
	   (string-prefix-p "*ccls::stderr*" name)
	   (string-prefix-p "*format-all-errors*" name)

	   ;; bash
	   (string-prefix-p "*bash-ls*" name)
	   (string-prefix-p "*bash-ls::stderr*" name)

	   ;; org
	   (string-prefix-p "*Org Preview LaTeX Output*" name)
	   (string-prefix-p "*elfeed-log*" name)

	   ;; python
	   (string-prefix-p "*pyright*" name)
	   (string-prefix-p "*pyright::stderr*" name)

	   ;; typescript
	   (string-prefix-p "*tide-server*" name)
	   (string-prefix-p "*ts-ls*" name)
	   (string-prefix-p "*ts-ls::stderr*" name)

	   ;; other
	   (string-prefix-p "*Native-compile-Log*" name)
	   (string-prefix-p "*Async-Native" name)
	   (string-prefix-p "*httpd*" name)
	   (string-prefix-p "*Shell Command Output" name)

	   ;; Is not magit buffer.
	   (and (string-prefix-p "magit" name)
			(not (file-name-extension name)))
	   )))

  ;; Binds
  (global-set-key (kbd "C-x t 2") 'centaur-tabs--create-new-tab)
  (global-set-key (kbd "C-x t 0") 'kill-buffer-and-window)
  (global-set-key (kbd "C-x t t") 'centaur-tabs-ace-jump)
  (global-set-key (kbd "C-x t h") 'centaur-tabs-move-current-tab-to-left)
  (global-set-key (kbd "C-x t l") 'centaur-tabs-move-current-tab-to-right)
  (global-set-key (kbd "C-x t C-w") 'kill-buffer-and-window)
  (global-set-key (kbd "C-x t <left>") 'centaur-tabs-backward)
  (global-set-key (kbd "C-x t <right>") 'centaur-tabs-forward)
  (global-set-key (kbd "C-x t C-<left>") 'centaur-tabs-move-current-tab-to-left)
  (global-set-key (kbd "C-x t C-<right>") 'centaur-tabs-move-current-tab-to-right)

  ;; (global-set-key (kbd "S-<f9>") 'toggle-centaur-tabs)
  (global-set-key (kbd "C-<tab>") 'centaur-tabs-forward)
  (global-set-key (kbd "C-<iso-lefttab>") 'centaur-tabs-backward)

  ;; Magit Binds
  (evil-define-key 'normal magit-mode-map (kbd "C-<tab>") #'centaur-tabs-forward)
  (evil-define-key 'normal magit-mode-map (kbd "C-<iso-lefttab>") #'centaur-tabs-backward)
  )

;; Automatically enable centaur-tabs
(defun auto-enable-centaur-tabs ()
  (interactive)
  (if (daemonp)
	  (add-hook 'after-make-frame-functions
				(lambda (frame)
				  (with-selected-frame frame
					(enable-centaur-tabs)))
				(enable-centaur-tabs))))
;; (auto-enable-centaur-tabs)

;; Clean up the modeline
(use-package diminish
  :straight t
  :init
  ;; diminish as mode is already loaded
  (diminish 'auto-revert-mode "")
  (diminish 'abbrev-mode "")
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
  (eval-after-load "typescript-mode" '(diminish 'subword-mode))
  (eval-after-load "js2-mode" '(diminish 'subword-mode ""))
  (eval-after-load "dired" '(diminish 'dired-async-mode))
  (eval-after-load "dired" '(diminish 'dired-hide-dotfiles-mode))
  (eval-after-load "dired" '(diminish 'all-the-icons-dired-mode))
  (eval-after-load "magit" '(diminish 'auto-fill-mode ""))
  (eval-after-load "magit" '(diminish 'with-editor-mode ""))
  (eval-after-load "slime" '(diminish 'slime-autodoc-mode ""))
  (eval-after-load "olivetti" '(diminish 'olivetti-mode ""))
  (eval-after-load "evil" '(diminish 'evil-collection-unimpaired-mode ""))
  (eval-after-load "mu4e" '(diminish 'overwrite-mode ""))
  (eval-after-load "mu4e" '(diminish 'mu4e-modeline-mode ""))
  (eval-after-load "auto-revert-mode" '(diminish 'auto-revert-mode "")))

;; Golden ratio for windows
(use-package golden-ratio
  :straight t
  :config
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

;; Colorize color-codes in prog-modes
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Highlight matching brackets
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Unicode emoji
(use-package emojify
  :straight t
  :defer 6
  :config
  (setq emojify-emoji-styles '(unicode)))

;; Make info-mode use variable-pitch fonts
(straight-use-package
 '(info-variable-pitch
   :type git :host github
   :repo "kisaragi-hiu/info-variable-pitch"))
(add-hook 'Info-mode-hook #'info-variable-pitch-mode)

;; Highlight indent guidelines
(use-package highlight-indent-guides
  :disabled t
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'js-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (add-hook 'js2-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (add-hook 'typescript-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (defun indent-guides-init-faces ()
	"Set indent-guides faces"
	(interactive)
	(set-face-background 'highlight-indent-guides-odd-face "gray30")
	(set-face-background 'highlight-indent-guides-even-face "gray30")
	(set-face-foreground 'highlight-indent-guides-character-face "gray30"))
  (defun indent-guides-dark-faces ()
	"Set VSCode themed indent-guides faces"
	(interactive)
	(set-face-background 'highlight-indent-guides-odd-face "#404040")
	(set-face-background 'highlight-indent-guides-even-face "#404040")
	(set-face-foreground 'highlight-indent-guides-character-face "#404040"))
  (indent-guides-init-faces))

;; Minimap
(use-package minimap
  :straight t
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
  (defun minimap/enable ()
	"Enable minimap."
	(interactive)
	(minimap-mode t)
	(put 'minimap-toggle 'state t))
  (defun minimap/disable ()
	"Disable minimap."
	(interactive)
	(minimap-kill)
	(put 'minimap-toggle 'state nil))
  (defun minimap/toggle ()
	"Toggle minimap."
	(interactive)
	(if (get 'minimap-toggle 'state)
		(minimap/disable)
	  (minimap/enable)))
  (defun minimap/refresh ()
	"Refresh minimap."
	(interactive)
	(minimap/disable)
	(minimap/enable))
  :config
  (setq minimap-window-location 'right)
  (setq minimap-width-fraction 0.05)
  (setq minimap-minimum-width 15)
  (setq minimap-hide-fringes t)
  ;;(setq minimap-major-modes '(typescript-mode typescriptreact-mode js-mode js2-mode))
  (setq minimap-major-modes '(prog-mode))
  ;;(setq minimap-update-delay 0)
  (light-minimap))

;; Project management
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
  (global-set-key (kbd "C-c p b") 'consult-project-buffer)
  (global-set-key (kbd "C-c p s") 'projectile-ag)
  (global-set-key (kbd "C-c p !") 'projectile-run-shell-command-in-root)
  (projectile-mode +1))

;; Rip-grep
(use-package rg
  :straight t)

;; Silver-searcher
(use-package ag
  :straight t)

;; Replace fancy major modes with basic major mode
;; when long lines are detected.
(use-package so-long
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

;; Input unicode symbols
(use-package xah-math-input
  :straight
  ( :repo "diamondbond/xah-math-input"
	:host github
	:type git
	:files ("xah-math-input.el")))

;; Diagnostics for elisp
(use-package flymake
  :straight t
  :config
  ;; Message navigation bindings
  (with-eval-after-load 'flymake
	(define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
	(define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))
  :hook ((emacs-lisp-mode) . flymake-mode))

;; Spellchecking
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
	;; treemacs-width                         35)
	;; (treemacs-resize-icons 11)

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
  ;; :disabled treemacs evil
  :straight t)

;; Neotree
(use-package neotree
  :disabled t
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
  (defun neotree/show ()
	"Show neotree."
	(interactive)
	(neotree-toggle-project-dir)
	(put 'neotree-toggle 'state t)
	(other-window 1))
  (defun neotree/hide ()
	"Hide neotree."
	(interactive)
	(neotree-hide)
	(put 'neotree-toggle 'state nil))
  (defun neotree/toggle ()
	"Toggle neotree."
	(interactive)
	(if (get 'neotree-toggle 'state)
		(neotree/hide)
	  (neotree/show)))
  :custom
  ;;(neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-theme 'icons)
  (neo-window-position 'left)
  (neo-window-width 30)
  (neo-show-hidden-files t))

;; Convert buffer text and decorations to HTML
(use-package htmlize
  :straight t)

;; VTerm
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

;; Mutliplex vterm
(if (eq system-type 'gnu/linux)
	(use-package multi-vterm
	  :straight t
	  :after vterm
	  :config
	  (add-hook 'vterm-mode-hook
				(lambda ()
				  ;;(setq-local evil-insert-state-cursor 'box)
				  (evil-insert-state)))))

;; Toggle vterm
(use-package vterm-toggle
  :straight t
  :config
  (setq vterm-toggle-fullscreen-p nil))
;; Dedicated size for vterm
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

;; Collection of ridiculously useful extensions
(use-package crux
  :straight t)

;; File upload to 0x0
(use-package 0x0
  :straight t
  :commands (0x0-dwim 0x0-popup 0x0-upload-file 0x0-upload-text))

;; Show commands being used
(use-package command-log-mode
  :straight t
  :defer 5
  :diminish command-log-mode)

;; Live Github style markdown previw
(use-package grip-mode
  :straight t
  :defer 1
  :bind (:map markdown-mode-command-map
			  ("g" . grip-mode)))

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

;; bindings to help improve the speed of magit
;; (use-package libgit :straight t)
;; (use-package magit-libgit :straight t)

;; Autorevert - required by magit
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
  :straight t
  ;; swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
		 ("C-M-/" . dabbrev-expand)))

;; Expand/highlight regions
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region)))

;; Display keybinds
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

;; GNU tags
(use-package gtags
  :straight t
  :config
  (setq xref-prompt-for-identifier nil))

;; Corfu completion framework
(use-package corfu
  :straight t
  :demand t
  :bind (:map corfu-map
			  ("<escape>". corfu-quit)
			  ("<return>" . corfu-insert)
			  ("C-h" . corfu-show-documentation)
			  ("M-l" . 'corfu-show-location)
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

;; Completion at point extensions
(use-package cape
  :straight t
  :bind (("M-i" . completion-at-point)
		 ("C-c x d" . cape-dabbrev)
		 ("C-c x f" . cape-file)
		 ("C-c x s" . cape-symbol)
		 ("C-c x i" . cape-ispell))
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
  (setq completion-styles '(orderless partial-completion basic)
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

   ;; ("C-c m" . consult-mode-command)
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
				 (window-parameters (mode-line-format . none))))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  (embark-collect-mode . embark-consult-preview-minor-mode))

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
  :straight t
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

;; Deadgrep
(use-package deadgrep
  :straight t
  :commands deadgrep)

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
;; :bind
;; ("C-c n d" . deft)))

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
;;(define-key evil-normal-state-map (kbd "RET") 'elfeed-search-show-entry))

;; Mailing lists
(use-package gnus
  :straight t
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
  :straight t
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
  :defer 2
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

;; Restore pdf view
(use-package pdf-view-restore
  :after pdf-tools
  :straight t
  :config
  :hook (pdf-view-mode . pdf-view-restore-mode))

;; Hugo markdown backend for org-export
(use-package ox-hugo
  :disabled t
  :after ox)

;; Academic phrases
(use-package academic-phrases
  :defer 5
  :straight t)

;; Polish up writing on the fly
(use-package writegood-mode
  :defer 4
  :straight t)

;; Thesaurus lookup
(use-package synosaurus
  :straight t)

;; Focus writing mode
(use-package olivetti
  :straight t
  :init
  (setq olivetti-body-width .75))

;; Save place in buffer
(use-package saveplace
  :straight t
  :defer nil
  :config
  (save-place-mode))

;; Save history
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; aLtCaPs
;; (use-package altcaps
;;   :straight (:type git :host github :repo "protesilaos/altcaps" :branch "main"))

(provide 'packages)
;;; packages.el ends here
