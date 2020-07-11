(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; EVIL ;; M-x package-install evil
(require 'evil)
(evil-mode 1)

;; Magit
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Org
(setq org-startup-with-inline-images t)
(setq initial-major-mode 'org-mode)
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

;; Which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Adjust window geometry
;;(if (display-graphic-p)
;;    (progn
;;      (setq initial-frame-alist
;;            '(
;;              (tool-bar-lines . 0)
;;              (width . 95) ; chars
;;              (height . 42) ; lines
;;              (left . 50)
;;              (top . 50)))
;;      (setq default-frame-alist
;;            '(
;;              (tool-bar-lines . 0)
;;              (width . 95)
;;              (height . 42)
;;			  (left . 50)
;;			  (top . 50))))
;;  (progn (setq initial-frame-alist '( (tool-bar-lines . 0)))
;;    (setq default-frame-alist '( (tool-bar-lines . 0)))))

(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (message nil))
(setq initial-scratch-message "")

;; Misc
;;(load-theme 'plan9 t)
;;(set-default-font "Ubuntu Mono-12")
(ido-mode 1)
;;(global-auto-revert-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(global-hl-line-mode 1)
(toggle-scroll-bar -1)
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

;; Resize bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; Toggles
(global-set-key (kbd "<f5>") 'menu-bar-mode)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#a60000" "#005e00" "#813e00" "#0030a6" "#721045" "#00538b" "#ffffff"])
 '(custom-safe-themes
   (quote
	("18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" default)))
 '(flymake-error-bitmap
   (quote
	(flymake-double-exclamation-mark modus-theme-fringe-red)))
 '(flymake-note-bitmap (quote (exclamation-mark modus-theme-fringe-cyan)))
 '(flymake-warning-bitmap (quote (exclamation-mark modus-theme-fringe-yellow)))
 '(hl-todo-keyword-faces
   (quote
	(("HOLD" . "#70480f")
	 ("TODO" . "#721045")
	 ("NEXT" . "#5317ac")
	 ("THEM" . "#8f0075")
	 ("PROG" . "#00538b")
	 ("OKAY" . "#30517f")
	 ("DONT" . "#315b00")
	 ("FAIL" . "#a60000")
	 ("DONE" . "#005e00")
	 ("NOTE" . "#863927")
	 ("KLUDGE" . "#813e00")
	 ("HACK" . "#813e00")
	 ("TEMP" . "#4d0006")
	 ("FIXME" . "#a0132f")
	 ("XXX+" . "#972500")
	 ("REVIEW" . "#005a5f")
	 ("DEPRECATED" . "#001170"))))
 '(ibuffer-deletion-face (quote modus-theme-mark-del))
 '(ibuffer-filter-group-name-face (quote modus-theme-mark-symbol))
 '(ibuffer-marked-face (quote modus-theme-mark-sel))
 '(ibuffer-title-face (quote modus-theme-header))
 '(package-selected-packages
   (quote
	(perspective all-the-icons treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs which-key use-package magit plan9-theme modus-vivendi-theme modus-operandi-theme pdf-tools evil)))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#a60000")
	 (40 . "#721045")
	 (60 . "#8f0075")
	 (80 . "#972500")
	 (100 . "#813e00")
	 (120 . "#70480f")
	 (140 . "#5d3026")
	 (160 . "#184034")
	 (180 . "#005e00")
	 (200 . "#315b00")
	 (220 . "#005a5f")
	 (240 . "#30517f")
	 (260 . "#00538b")
	 (280 . "#093060")
	 (300 . "#0030a6")
	 (320 . "#223fbf")
	 (340 . "#0000bb")
	 (360 . "#5317ac"))))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#a60000" "#005e00" "#813e00" "#0030a6" "#721045" "#00538b" "#f0f0f0"])
 '(xterm-color-names-bright
   ["#505050" "#972500" "#315b00" "#70480f" "#223fbf" "#8f0075" "#30517f" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
