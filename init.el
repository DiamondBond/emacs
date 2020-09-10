;; Make emacs startup faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
 
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))
 
(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
    gc-cons-percentage 0.1))
 
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;

;; Initialize melpa repo
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load config.org for init.el configuration
;;(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(org-babel-load-file (expand-file-name "~/Dropbox/emacs/config.org"))

;;(load-theme 'spacemacs-light t)
(load-theme 'acme t)
;; Disable colored bg for terminal
(defun on-frame-open (&optional frame)
  "If the frame was created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame))
)
(add-hook 'after-make-frame-functions 'on-frame-open)

;; Start emacs server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-file-apps
   '((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . "zathura %s")))
 '(package-selected-packages
   '(pdf-view-restore acme-theme nyan-mode org-pdftools synosaurus xah-math-input ivy flycheck swiper-helm writeroom-mode restart-emacs fireplace pdf-tools spacemacs-theme plan9-theme modus-vivendi-theme modus-operandi-theme meghanada company-irony company-c-headers yasnippet-snippets yasnippet company magit treemacs-icons-dired treemacs-evil treemacs async ido-vertical-mode switch-window avy beacon evil swiper which-key dashboard spaceline diminish auto-package-update htmlize use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco")))))
