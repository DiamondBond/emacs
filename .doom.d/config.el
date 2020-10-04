;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Diamond Bond"
      user-mail-address "diamondbond1@gmail.com")
(setq org-directory "~/org/")
(setq display-line-numbers-type t)
(setq doom-font (font-spec :family "Monaco" :size 12))
;;(setq doom-big-font (font-spec :family "Monaco" :size 20))
;;(setq doom-theme 'doom-one-light)
;;(load-theme 'spacemacs-light t)
;;(set-face-background 'default "#FFFFFF")
;; or
;;(set-face-attribute  'default nil :background "#FFFFFF")
(load-theme 'modus-operandi t)
(display-battery-mode 1)

;;(setq doom-font (font-spec :family "monospace" :size 14 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
;;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;(display-time-mode 1)
;;(display-battery-mode 1)

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))
;; Don't need the menu
;;(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;(tool-bar-mode 1)
;;(menu-bar-mode 1)
;;(scroll-bar-mode 1)

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

(global-set-key (kbd "<f9>") 'tab-bar-mode)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'menu-bar-mode)
(global-set-key (kbd "<f7>") 'scroll-bar-mode)
(global-set-key (kbd "<f8>") 'tool-bar-mode)
(global-set-key (kbd "<f12>") 'linum-mode)
;;(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "C-x w") 'elfeed)
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

(use-package! diminish)

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
  (setq dashboard-startup-banner "~/.doom.d/splash.png")
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
