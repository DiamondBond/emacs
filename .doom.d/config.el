;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Diamond Bond"
      user-mail-address "diamondbond1@gmail.com")
;;(setq org-directory "~/org/")
(setq display-line-numbers-type t)
;;(setq doom-font (font-spec :family "Ubuntu Mono" :size 14))
;;(setq doom-big-font (font-spec :family "Ubuntu Mono" :size 24))

;;(setq doom-font (font-spec :family "DejaVu Sans Mono"))

;; Main typeface
;;(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)

;;(set-face-attribute 'default nil :height (if (string-equal system-name "phoenix") 140 110))

;; Proportionately spaced typeface
;;(set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :height 1.0)

;; Monospaced typeface
                                        ;(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 1.0)

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 16))
;; (setq doom-big-font (font-spec :family "JetBrains Mono" :size 24))
;; (setq doom-variable-pitch-font (font-spec :family "Overpass" :size 16))
;; (setq doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;;(load-theme 'spacemacs-light t)
;;(set-face-background 'default "#FFFFFF")
;; or
;;(set-face-attribute  'default nil :background "#FFFFFF")

;;(setq custom-safe-themes t)
;;(if (display-graphic-p)
;;    (load-theme 'modus-operandi t)
;;  (load-theme 'doom-dracula t))

;;(load-theme 'acme t)
;;(load-theme 'modus-operandi t)
;;(load-theme 'doom-dracula t)
;;(display-battery-mode 1)

(setq doom-font (font-spec :family "monospace" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 14))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;(display-time-mode 1)
;;(display-battery-mode 1)

;;; :ui doom-dashboard
;;(setq fancy-splash-image (concat doom-private-dir "splash.png"))
;; Don't need the menu
;;(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(setq-default
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      ;; auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t)           ; When there are lots of glyphs, keep them in memory

(delete-selection-mode 1)                         ; Replace selection when inserting text
;; (global-subword-mode 1)                           ; Iterate through CamelCase words

(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;(tool-bar-mode 1)
;;(menu-bar-mode 1)
;;(menu-bar-mode (if (string-equal system-name "x220") 1 0))
;;(scroll-bar-mode (if (string-equal system-name "x220") 1 0))
(scroll-bar-mode 1)
;;(setq +modeline-height 36)
;;(setq doom-fallback-buffer-name "emacs"
;;      +doom-dashboard-name "emacs")
;;(setq-default frame-title-format '("%b"))

;; (setq evil-split-window-below t
;;       evil-vsplit-window-right t)

  (setq scroll-conservatively 1)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)

  (global-prettify-symbols-mode t)

  (global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "s-C-<down>") 'shrink-window)
  (global-set-key (kbd "s-C-<up>") 'enlarge-window)

(setq-default frame-title-format '("%b"))

(global-hl-line-mode t)

(set-popup-rule! "^\\*eww.*" :size 82 :side 'right :select t :quit t)

;; (set-popup-rule! "^\\*Customize.*" :slot 2 :side 'right :modeline nil :select t :quit t)
;; (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'left :size 20 :modeline nil :select t :quit t)
;; (set-popup-rule! "^\\*Password-Store" :side 'left :size 0.25)

;; ;; * help
;; (set-popup-rule! "^\\*info.*" :size 82 :side 'right :ttl t :select t :quit t)
;; (set-popup-rule! "^\\*Man.*" :size 82 :side 'right :ttl t :select t :quit t)
;; (set-popup-rule! "^\\*tldr\\*" :size 82 :side 'right :select t :quit t)
;; (set-popup-rule! "^\\*helpful.*" :size 82 :side 'right :select t :quit t)
;; (set-popup-rule! "^\\*Help.*" :size 82 :height 0.6 :side 'right :select t :quit t)
;; (set-popup-rule! "^ \\*Metahelp.*" :size 82 :side 'right :select t :quit t)
;; (set-popup-rule! "^\\*Apropos.*" :size 82 :height 0.6 :side 'right :select t :quit t)
;; (set-popup-rule! "^\\*Messages\\*" :vslot -10 :height 10 :side 'bottom :select t :quit t :ttl nil)

;; ;; (set-popup-rule! "^ ?\\*NeoTree" :side ,neo-window-position :width ,neo-window-width :quit 'current :select t)
;; (set-popup-rule! "\\*VC-history\\*" :slot 2 :side 'right :size 82 :modeline nil :select t :quit t)

;; ;; * web
;; (set-popup-rule! "^\\*eww.*" :size 82 :side 'right :select t :quit t)
;; (set-popup-rule! "\\*xwidget" :side 'right :size 100 :select t)

;; ;; * lang
;; ;; ** python
;; (set-popup-rule! "^\\*Anaconda\\*" :side 'right :size 82 :quit t :ttl t)
;; ;; ** R
;; (after! ess-r-mode
;;   (set-popup-rule! "^\\*R:.*\\*" :side 'bottom :slot -1 :height 0.6 :width 0.5 :select nil :quit nil :ttl nil))
;; (after! ess-help
;;   (set-popup-rule! "^\\*help.R.*" :slot 2 :side 'right :size 80 :height 0.4 :select t :quit t :transient t))

;; (after! org
;;   (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil :select t))

;; my own map
(define-prefix-command 'z-map)
(global-set-key (kbd "C-1") 'z-map) ;; was C-1
(define-key z-map (kbd "f") 'find-file-other-frame)
(define-key z-map (kbd "d") 'dired-other-frame)
(define-key z-map (kbd "g") '+default/search-cwd)
(define-key z-map (kbd "G") 'org-mark-ring-goto)
(define-key z-map (kbd "2") 'make-frame-command)
(define-key z-map (kbd "0") 'delete-frame)
(define-key z-map (kbd "o") 'other-frame)

(define-key z-map (kbd "*") 'calc)
(define-key z-map (kbd "r") 'synosaurus-choose-and-replace)
(define-key z-map (kbd "R") 'rainbow-mode)
(define-key z-map (kbd "O") 'org-redisplay-inline-images)
(define-key z-map (kbd "s") 'ispell-word)
(define-key z-map (kbd "W") 'elfeed)
(define-key z-map (kbd "w") 'eww)
(define-key z-map (kbd "p") #'+popup/raise)
(define-key z-map (kbd "F") 'browse-url-firefox)

(define-key z-map (kbd "h") 'hyperbole)
(define-key z-map (kbd "X") 'xah-math-input-mode)
(define-key z-map (kbd "x") 'switch-to-buffer-other-frame)
(define-key z-map (kbd "k") 'compile)
(define-key z-map (kbd "e") 'eval-region)

;;(define-key z-map (kbd "b") 'burly-open-bookmark)
;;(define-key z-map (kbd "B") 'burly-bookmark-frames)

(define-key z-map (kbd "a") '(lambda () (interactive) (find-file-other-window "~/org/agenda.org")))
(define-key z-map (kbd "C-c") 'calendar)
(define-key z-map (kbd ".") 'org-date-from-calendar)
(define-key z-map (kbd "C-o") 'olivetti-mode)

(define-key z-map (kbd "I") (lambda () (interactive) (find-file-other-window "~/org/index.org")))
(define-key z-map (kbd "N") (lambda () (interactive) (find-file-other-window "~/org/notes.org")))
(define-key z-map (kbd "C") (lambda () (interactive) (find-file-other-window "~/.doom.d/config.org")))

(define-key z-map (kbd "i") (lambda () (interactive) (find-file "~/org/index.org")))
(define-key z-map (kbd "n") (lambda () (interactive) (find-file "~/org/notes.org")))
(define-key z-map (kbd "c") (lambda () (interactive) (find-file "~/.doom.d/config.org")))

(define-key z-map (kbd "K") 'keycast-mode)
(define-key z-map (kbd "R") 'gif-screencast-start-or-stop)

;;---------------------------------------------------------------------

(global-set-key (kbd "<f9>") 'tab-bar-mode)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'menu-bar-mode)
(global-set-key (kbd "<f7>") 'scroll-bar-mode)
(global-set-key (kbd "<f8>") 'tool-bar-mode)
(global-set-key (kbd "<f12>") 'linum-mode)
;;(global-set-key (kbd "<f10>") 'compile)
;;(global-set-key (kbd "C-x w") 'elfeed)
;;(global-set-key (kbd "SPC h h") 'hyperbole)
(global-set-key (kbd "M-s") 'avy-goto-char)

(global-set-key (kbd "C-x x") 'window-swap-states)

(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")
(setq org-agenda-files (list "inbox.org"))
(global-set-key (kbd "C-<f1>") (lambda()
                                 (interactive)
                                 (show-all)))

;; src exec
(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (shell . t)
                               )
                             )

(setq org-directory "~/org"
      org-image-actual-width nil
      +org-export-directory "~/org/export"
      org-default-notes-file "~/org/inbox.org"
      org-id-locations-file "~/org/.orgids"
      org-agenda-files (directory-files-recursively "~/Dropbox/org/" "\\.org$")
      ;; org-export-in-background t
      org-catch-invisible-edits 'smart)

;; (setq org-todo-keywords
;;       '((sequence "TODO" "WIP" "WAIT" "DONE")))

(setq org-roam-directory "~/org/roam")

(setq deft-directory "~/org/"
      deft-recursive t
      ;; I don't like any summary, hence catch-all regexp. need to see if
      ;; an option to hide summary is there instead of this one.
      deft-strip-summary-regexp ".*$"
      )

;; scratch is now org
(setq initial-major-mode 'org-mode)

  (defalias 'open 'find-file-other-window)
  (defalias 'clean 'eshell/clear-scrollback)

  (defun eshell/sudo-open (filename)
    "Open a file as root in Eshell."
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename)))))

  (defun eshell-other-window ()
    "Create or visit an eshell buffer."
    (interactive)
    (if (not (get-buffer "*eshell*"))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (eshell))
      (switch-to-buffer-other-window "*eshell*")))

  (global-set-key (kbd "<s-C-return>") 'eshell-other-window)

;;(use-package! diminish)

;; (rich-minority-mode 1)
;; (setq rm-blacklist
;;       (format "^ \\(%s\\)$"
;;               (mapconcat #'identity
;;                          '("Fly.*" "Projectile.*" "PgLn" "traces" "snipe" "WK" "better-jumper" "company" "ivy" "EG" "GCMH" "SP" "EvilOrg" "~" "$" "jk" "wb" "ws" "Outl" "ElDoc" "yas" "Ind" "FmtAll" "Wrap" "GitGutter" "dtrt-indent" "Abbrev")
;;                          "\\|")))

 (setq rm-blacklist "")
 (rich-minority-mode)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

;;(use-package! spaceline)

 ;; (use-package! powerline
 ;;    :init
 ;;    (spaceline-spacemacs-theme)
 ;;    :hook
 ;;    ('after-init-hook) . 'powerline-reset)

(use-package! dashboard
  :defer nil
  :preface
  (defun init-edit ()
    "Edit initialization file"
    (interactive)
    (find-file "~/.doom.d/init.el"))
  (defun config-edit ()
    "Edit configuration file"
    (interactive)
    (find-file "~/.doom.d/config.org"))
  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
;;  (setq dashboard-startup-banner "~/.doom.d/splash.png")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(((,nil
            "Scratch Buffer"
            "Switch to the scratch buffer"
            (lambda (&rest _) (create-scratch-buffer))
            'default)
           (nil
            "Config.org"
            "Open Emacs configuration file for easy editing"
            (lambda (&rest _) (config-edit))
            'default)))))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (use-package! switch-window
	:config
	(setq switch-window-input-style 'minibuffer)
	(setq switch-window-increase 4)
	(setq switch-window-threshold 2)
	(setq switch-window-shortcut-style 'qwerty)
	(setq switch-window-qwerty-shortcuts
		  '("a" "s" "d" "f" "j" "k" "l"))
	:bind
	([remap other-window] . switch-window))

  (setq elfeed-feeds
      '((("https://www.gnome.org/feed/" gnu de)
        ("https://planet.emacslife.com/atom.xml" emacs community)
        ("https://www.ecb.europa.eu/rss/press.html" economics eu)
		  ("https://news.ycombinator.com/rss" ycombinator news)
		  ("https://www.phoronix.com/rss.php" phoronix))))

  (use-package! saveplace
	  :defer nil
    :config
    (save-place-mode))

(after! which-key
    (setq which-key-idle-delay 0.5))

(after! magit
  ;; (magit-wip-mode)
  (setq magit-repository-directories '(("~/git" . 2))
        magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit
        magit-inhibit-save-previous-winconf t
        magit-log-arguments '("--graph" "--decorate" "--color")
        ;; magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 120))

(after! latex
    (setq org-latex-compiler "xelatex"))

(use-package emacs
  :config
  (setq custom-safe-themes t)

  ;; TODO simplify this to avoid formatting a string, then read and eval.
  (defmacro modus-themes-format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))

  (defvar modus-themes-after-load-hook nil
    "Hook that runs after loading a Modus theme.
See `modus-operandi-theme-load' or `modus-vivendi-theme-load'.")

  (dolist (theme '("operandi" "vivendi"))
    (modus-themes-format-sexp
     (defun modus-%1$s-theme-load ()
       (setq modus-%1$s-theme-slanted-constructs t
             modus-%1$s-theme-bold-constructs nil
             modus-%1$s-theme-fringes nil ; {nil,'subtle,'intense}
             modus-%1$s-theme-mode-line nil ; {nil,'3d,'moody}
             modus-%1$s-theme-syntax nil ; {nil,faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}
             modus-%1$s-theme-intense-hl-line nil
             modus-%1$s-theme-intense-paren-match nil
             modus-%1$s-theme-links 'neutral-underline ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
             modus-%1$s-theme-no-mixed-fonts nil
             modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
             modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
             modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
             modus-%1$s-theme-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}
             modus-%1$s-theme-headings  ; Read the manual for this one
             '((t . nil))
             modus-%1$s-theme-variable-pitch-headings t
             modus-%1$s-theme-scale-headings nil
             modus-%1$s-theme-scale-1 1.1
             modus-%1$s-theme-scale-2 1.15
             modus-%1$s-theme-scale-3 1.21
             modus-%1$s-theme-scale-4 1.27
             modus-%1$s-theme-scale-5 1.33)
       (load-theme 'modus-%1$s t)
       (run-hooks 'modus-themes-after-load-hook))
     theme))

  (defun modus-themes-light ()
    "Load `modus-operandi' and disable `modus-vivendi'."
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load))

  (defun modus-themes-dark ()
    "Load `modus-vivendi' and disable `modus-operandi'."
    (disable-theme 'modus-operandi)
    (modus-vivendi-theme-load))

  (defun modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (modus-themes-dark)
      (modus-themes-light)))

  :hook (after-init-hook . modus-operandi-theme-load)
  :bind ("<S-f5>" . modus-themes-toggle))

(modus-themes-toggle)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

;; (require 'exwm)
;; (require 'exwm-config)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
;; (require 'exwm-randr)
;; (exwm-randr-enable)
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output eDP-1 --mode 1920x1080 --pos 0x0 --rotate normal")))
;; (setq exwm-workspace-number 10
;;       exwm-randr-workspace-output-plist '(0 "eDP-1")
;;       exwm-input-prefix-keys '(?\M-x
;;                                ?\M-:)
;;       exwm-input-simulation-keys '(([?\s-F] . [?\C-f])
;;                                    )
;;       exwm-input-global-keys '(([?\s-&] . (lambda (command)
;;                                             (interactive (list (read-shell-command "$ ")))
;;                                             (start-process-shell-command command nil command)))
;;                                ;; splits
;;                                ([?\s-v] . evil-window-vsplit)
;;                                ([?\s-z] . evil-window-split)
;;                                ;; managing workspaces
;;                                ([?\s-w] . exwm-workspace-switch)
;;                                ([?\s-W] . exwm-workspace-swap)
;;                                ([?\s-\C-w] . exwm-workspace-move)
;;                                ;; essential programs
;;                                ([?\s-d] . dired)
;;                                ([s-return] . vterm)
;;                                ([s-S-return] . dmenu)
;;                                ;; killing buffers and windows
;;                                ([?\s-b] . ibuffer)
;;                                ([?\s-B] . kill-current-buffer)
;;                                ([?\s-C] . +workspace/close-window-or-workspace)
;;                                ;; change window focus with super+h,j,k,l
;;                                ([?\s-h] . evil-window-left)
;;                                ([?\s-j] . evil-window-next)
;;                                ([?\s-k] . evil-window-prev)
;;                                ([?\s-l] . evil-window-right)
;;                                ;; move windows around using SUPER+SHIFT+h,j,k,l
;;                                ([?\s-H] . +evil/window-move-left)
;;                                ([?\s-J] . +evil/window-move-down)
;;                                ([?\s-K] . +evil/window-move-up)
;;                                ([?\s-L] . +evil/window-move-right)
;;                                ;; move window to far left or far right with SUPER+CTRL+h,l
;;                                ([?\s-\C-h] . side-left-window)
;;                                ([?\s-\C-j] . side-bottom-window)
;;                                ([?\s-\C-l] . side-right-window)
;;                                ([?\s-\C-d] . side-window-delete-all)
;;                                ([?\s-\C-r] . resize-window)
;;                                ;; switch workspace with SUPER+{0-9}
;;                                ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
;;                                ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
;;                                ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
;;                                ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
;;                                ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
;;                                ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
;;                                ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
;;                                ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
;;                                ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
;;                                ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 9)))
;;                                ;; move window workspace with SUPER+SHIFT+{0-9}
;;                                ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
;;                                ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
;;                                ([?\s-@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
;;                                ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
;;                                ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
;;                                ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
;;                                ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
;;                                ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
;;                                ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
;;                                ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
;;                                ;; setting some toggle commands
;;                                ([?\s-f] . exwm-floating-toggle-floating)
;;                                ([?\s-m] . exwm-layout-toggle-mode-line)
;;                                ([f11] . exwm-layout-toggle-fullscreen)))

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
        (:map dired-mode-map
         :leader
         :desc "Peep-dired image previews"
         "d p" #'peep-dired
         :leader
         :desc "Dired view file"
         "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(require 'emms-setup)
(require 'emms-info)
(require 'emms-cue)
(require 'emms-mode-line)
(require 'emms-playing-time)
(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/Non-Classical/70s-80s/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      :desc "Go to emms playlist"
      "a a" #'emms-playlist-mode-go
      :leader
      :desc "Emms pause track"
      "a x" #'emms-pause
      :leader
      :desc "Emms stop track"
      "a s" #'emms-stop
      :leader
      :desc "Emms play previous track"
      "a p" #'emms-previous
      :leader
      :desc "Emms play next track"
      "a n" #'emms-next)

;;(require 'ivy-posframe)

;; display at `ivy-posframe-style'
;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))

;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;;(ivy-posframe-mode 1)

;; (setq ivy-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))

;; (map! :leader
;;       :desc "Ivy push view"
;;       "v p" #'ivy-push-view
;;       :leader
;;       :desc "Ivy switch view"
;;       "v s" #'ivy-switch-view)

(add-to-list 'load-path "/home/diamond/.doom.d/el-go")
(require 'go)
