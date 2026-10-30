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

;; Define when to check for package modifications,
;; for improved straight.el startup time.
(setq straight-check-for-modifications '(check-on-save))

;; Use the versions Emacs ships instead of cloning these as dependencies.
(setq straight-built-in-pseudo-packages
	  '(emacs nadvice python image-mode use-package bind-key project xref
			  eldoc flymake jsonrpc eglot so-long which-key erc))

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

;; use-package is built in; loading it activates straight's :straight keyword
(require 'use-package)

;; Configure straight.el
(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-clone-depth 1)
  (straight-recipes-gnu-elpa-use-mirror t)
  (straight-check-for-modifications '(check-on-save)))

;; Config modules live in lisp/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
 globals--scratch      "~/org/scratch.md"                   ; Scratch
 )

;;---------------------------------------------------------------------
;; BASE
;;---------------------------------------------------------------------

(require 'base)

;;---------------------------------------------------------------------
;; KEYBINDS
;;---------------------------------------------------------------------

(require 'keybinds)

;;---------------------------------------------------------------------
;; PACKAGES
;;---------------------------------------------------------------------

(require 'packages)

;;---------------------------------------------------------------------
;; LANGUAGES
;;---------------------------------------------------------------------

(require 'languages)

;;---------------------------------------------------------------------
;; FUNCTIONS
;;---------------------------------------------------------------------

(require 'functions)

;;=====================================================================
;;============================== MODULES ==============================
;;=====================================================================

;;---------------------------------------------------------------------
;; THEME
;;---------------------------------------------------------------------

(load-theme-based-on-system-theme)

;;---------------------------------------------------------------------
;; END
;;---------------------------------------------------------------------

;;; init.el ends here
