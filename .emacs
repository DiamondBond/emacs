(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Startup
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (message nil))
(setq initial-scratch-message "")

;; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; Defer loading most packages for quicker startup time
;;(setq use-package-always-defer t)

;; EVIL
(require 'evil)
(evil-mode 1)

;; EXWM
;;(require 'exwm)
;;(require 'exwm-config)
;;(exwm-config-default)
;;(require 'exwm-systemtray)
;;(exwm-systemtray-enable)
;;(global-set-key (kbd "s-k") 'exwm-workspace-delete)
;;(global-set-key (kbd "s-w") 'exwm-workspace-swap)

;; Org
;;(setq org-startup-with-inline-images t)
;;
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")
;;
(setq org-directory "~/org/")
(setq initial-major-mode 'org-mode)
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
(global-set-key (kbd "C-<f1>") (lambda()
								 (interactive)
								 (show-all)))
;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Centaur tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
;;  :hook
;;  (dired-mode . centaur-tabs-local-mode)
;;  (dashboard-mode . centaur-tabs-local-mode)
;;  (term-mode . centaur-tabs-local-mode)
;;  (calendar-mode . centaur-tabs-local-mode)
;;  (org-agenda-mode . centaur-tabs-local-mode)
;;  (helpful-mode . centaur-tabs-local-mode)
  :bind
  (:map evil-normal-state-map
		("g t" . centaur-tabs-forward)
		("g T" . centaur-tabs-backward))
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
(centaur-tabs-headline-match)
(setq centaur-tabs-cycle-scope 'tabs)
(setq centaur-tabs-set-close-button nil)
;;(setq centaur-tabs-show-navigation-buttons t)

;; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Misc
(setenv "BROWSER" "firefox")t
(set-default-font "Ubuntu Mono-12")
(load-theme 'modus-operandi t)
(ido-mode 1)
;;(global-auto-revert-mode 1)
;;
(tool-bar-mode -1)
;;(menu-bar-mode -1)
;;(toggle-scroll-bar -1)
;;
;;(global-hl-line-mode 1)
;;(setq-default truncate-lines t) 
(column-number-mode 1)
(setq major-mode 'text-mode)
(setq-default major-mode 'text-mode)
(global-visual-line-mode 1)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-prettify-symbols-mode t)
(setq scroll-conservatively 100)
(transient-mark-mode 1)
(setq ring-bell-function 'ignore)

;; Aliases
(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Indentation
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'nil)

;; Bracket Completion
(show-paren-mode 1)
(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

;; Eshell Prompt
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
(setq eshell-prompt-function
      (lambda nil
        (concat
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face `(:foreground "#99CCFF"))
           (replace-regexp-in-string
            (getenv "HOME")
            (propertize "~" 'face `(:foreground "#99CCFF"))
            (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
         (if (= (user-uid) 0)
             (propertize " α " 'face `(:foreground "#FF6666"))
         (propertize " λ " 'face `(:foreground "#A6E22E"))))))
(setq eshell-highlight-prompt nil)

;; Super - Control - RET to open eshell
(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 0)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))
(global-set-key (kbd "<C-s-return>") 'eshell-other-window)

;; Split Compilation Buffer
;;(setq special-display-buffer-names
;;      '("*compilation*"))
;;(setq special-display-function
;;      (lambda (buffer &optional args)
;;        (split-window)
;;        (switch-to-buffer buffer)
;;        (get-buffer-window buffer 0)))

;; Switch focus when splitting
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(defun split-and-follow-vertically ()
  (interactive)(split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Resize bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; Swap windows
(global-set-key (kbd "C-x x") 'window-swap-states)


;; Line settings
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(global-set-key "\C-x\ g" 'toggle-truncate-lines)

;; Toggles
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'menu-bar-mode)
(global-set-key (kbd "<f7>") 'scroll-bar-mode)
(global-set-key (kbd "<f8>") 'tool-bar-mode)
(global-set-key (kbd "<f12>") 'linum-mode)
(global-set-key (kbd "<f1>")
                (lambda () (interactive)
                  (load-theme 'modus-operandi t)))
(global-set-key (kbd "<f2>")
                (lambda () (interactive)
                  (load-theme 'modus-vivendi t)))
(global-set-key (kbd "<f10>") 'disable-theme)
(global-set-key (kbd "C-x w") 'elfeed)

;; Browser (eww)
(setq browse-url-browser-function 'eww-browse-url)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 16)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
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

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; all-the-icons
(use-package all-the-icons)

;; lsp
(use-package lsp-mode
  :ensure t
  :hook
  ((c++-mode . lsp)
  (c-mode . lsp))
  :commands lsp)

(use-package ccls
  :after lsp-mode
  :ensure t
  :config (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
     (lambda () (require 'ccls))))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :commands company-lsp)

;; company
(use-package company
  :ensure t
  :commands company-mode
  :init
  (use-package company-quickhelp
	:ensure t)
  :config
  (company-quickhelp-mode))

;; pdf-tools
(use-package pdf-tools
   :pin manual ;; manually update
   :config
   ;; initialise
   (pdf-tools-install)
   ;; open pdfs scaled to fit width
   (setq-default pdf-view-display-size 'fit-width)
   ;; use normal isearch
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; openwith
;;(when (require 'openwith nil 'noerror)
;;(setq openwith-associations
;;    (list
;;     (list (openwith-make-extension-regexp
;;            '("mpg" "mpeg" "mp3" "mp4"
;;              "avi" "wmv" "wav" "mov" "flv"
;;              "ogm" "ogg" "mkv"))
;;           "mpv"
;;           '(file))
;;     (list (openwith-make-extension-regexp
;;            '("xbm" "pbm" "pgm" "ppm" "pnm"
;;              "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;           "nomacs"
;;           '(file))
;;     (list (openwith-make-extension-regexp
;;            '("doc" "docx" "xls" "xlsx" "ppt" "pptx" "odt" "ods" "odg" "odp"))
;;           "libreoffice"
;;           '(file))
;;     '("\\.lyx" "lyx" (file))
;;     '("\\.chm" "kchmviewer" (file))
;;     (list (openwith-make-extension-regexp
;;            '("pdf" "epub" "ps" "ps.gz" "dvi"))
;;           "zathura"
;;           '(file))
;;     ))
;;(openwith-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
	("https://news.ycombinator.com/rss" "https://www.reddit.com/r/linux.rss" "https://opensource.com/feed" "https://itsfoss.com/feed/" "https://www.phoronix.com/rss.php")))
 '(package-selected-packages
   (quote
	(centaur-tabs company-quickhelp company-lsp ccls lsp-mode olivetti org-bullets htmlize zenburn-theme xkcd wttrin which-key vterm use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil tao-theme speed-type poet-theme plan9-theme perspective pdf-tools parchment-theme openwith monokai-theme moe-theme modus-vivendi-theme modus-operandi-theme lorem-ipsum gruvbox-theme fireplace faff-theme eww-lnum epresent elfeed color-theme-modern cloud-theme ample-zen-theme ample-theme all-the-icons alect-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
