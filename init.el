;; Make emacs startup faster
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;; Initialize repos
(require 'package)
(setq package-archives nil)

(defvar org-archive '("org" . "https://orgmode.org/elpa/"))
(defvar gnu-archive '("elpa" . "https://elpa.gnu.org/packages/"))
(defvar melpa-archive '("melpa" . "https://melpa.org/packages/"))
(defvar nelpa-archive '("nelpa" . "https://elpa.nongnu.org/nongnu/"))
(defvar marmalade-archive '("marmalade" . "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives org-archive)
(add-to-list 'package-archives gnu-archive)
(add-to-list 'package-archives nelpa-archive)
(add-to-list 'package-archives melpa-archive)
;;(add-to-list 'package-archives marmalade-archive)

(setq package-archives (nreverse package-archives))
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Load config
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Supress cl warnings
(setq byte-compile-warnings '(cl-functions))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-file-apps
   '((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . emacs)))
 '(warning-suppress-log-types '((use-package) (browse-url) (comp)))
 '(warning-suppress-types '((use-package) (browse-url) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
