;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; EVIL ;; M-x package-install evil
(require 'evil)
(evil-mode 1)

;; Startup
;;(load-theme 'plan9)
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 95) ; chars
              (height . 42) ; lines
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 95)
              (height . 42)
			  (left . 50)
			  (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (message nil))
(setq initial-scratch-message "")

;; Misc
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

;; Bracket completion
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

;; Easier resize bindings
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

;; Org
(setq org-startup-with-inline-images t)
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" default)))
 '(org-agenda-files (quote ("~/org/org4beginners.org")))
 '(package-selected-packages
   (quote
	(plan9-theme xelb pdf-tools org modus-vivendi-theme modus-operandi-theme htmlize evil dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
