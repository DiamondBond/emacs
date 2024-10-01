;;; init.el --- Initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Diamond Bond
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Diamond Bond <diamondbond1@gmail.com>
;; URL: https://github.com/diamondbond/emacs
;; Package-Requires: ((emacs "29"))

;;; Code:

;;---------------------------------------------------------------------
;; CORE
;;---------------------------------------------------------------------

;; Make emacs startup faster
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  "Revert file name handler alist."
  (setq file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;; For performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq process-adaptive-read-buffering nil)

;; Fix for font-based performance issues
(setq inhibit-compacting-font-caches t)

;; Load newer .elc or .el
(setq load-prefer-newer t)

;; Initialize melpa repo
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Define when to check for package modifications,
;; for improved straight.el startup time.
(setq straight-check-for-modifications '(check-on-save))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package with straight.el
(straight-use-package 'use-package)

;; Configure straight.el
(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-clone-depth 1)
  (straight-recipes-gnu-elpa-use-mirror t)
  (straight-check-for-modifications '(check-on-save)))
;;(straight-check-for-modifications nil))

;;---------------------------------------------------------------------
;; GLOBALS
;;---------------------------------------------------------------------

(setq
 globals--font         "Menlo 11"                           ; Font
 globals--name         "Diamond Bond"                       ; Name
 globals--email        "diamondbond1@gmail.com"             ; Email
 globals--irc          "diamondbond"                        ; IRC
 globals--erc          '("diamondbond" "diamondbond_")      ; ERC
 globals--auth-info    "~/.authinfo.gpg"                    ; Auth Info
 globals--auth-sources '("~/.authinfo.gpg")                 ; Auth Src
 globals--browser      'browse-url-firefox                  ; Browser
 globals--banner-path  "img/gnu.png"                        ; Banner
 )

;;---------------------------------------------------------------------
;; BASE
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/base.el")
  (load-file "~/.emacs.d/lisp/base.el"))

;;---------------------------------------------------------------------
;; KEYBINDS
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/keybinds.el")
  (load-file "~/.emacs.d/lisp/keybinds.el"))

;;---------------------------------------------------------------------
;; PACKAGES
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/packages.el")
  (load-file "~/.emacs.d/lisp/packages.el"))

;;---------------------------------------------------------------------
;; LANGUAGES
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/languages.el")
  (load-file "~/.emacs.d/lisp/languages.el"))

;;---------------------------------------------------------------------
;; FUNCTIONS
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/functions.el")
  (load-file "~/.emacs.d/lisp/functions.el"))

;;=====================================================================
;;============================== MODULES ==============================
;;=====================================================================

;;---------------------------------------------------------------------
;; THEME
;;---------------------------------------------------------------------

(load-theme-based-on-system-theme)

;;---------------------------------------------------------------------
;; MAIL
;;---------------------------------------------------------------------

;; (when (file-readable-p "~/.emacs.d/lisp/mail.el")
;;   (load-file "~/.emacs.d/lisp/mail.el"))

;;---------------------------------------------------------------------
;; JIRA
;;---------------------------------------------------------------------

;; (when (file-readable-p "~/.emacs.d/lisp/jira.el")
;;   (load-file "~/.emacs.d/lisp/jira.el"))

;;---------------------------------------------------------------------
;; GPT
;;---------------------------------------------------------------------

(when (file-readable-p "~/.emacs.d/lisp/gpt.el")
  (load-file "~/.emacs.d/lisp/gpt.el"))

;;---------------------------------------------------------------------
;; END
;;---------------------------------------------------------------------

;; Restore desired GC values
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold lsp-cons-threshold)
			(setq gc-cons-percentage gc-cons-percentage-original)))

;;; init.el ends here
