;;; init.el --- Bootstrap -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Diamond Bond
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Diamond Bond <diamondbond1@gmail.com>
;; URL: https://github.com/diamondbond/emacs
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This file provides the bootstrap configuration.

;;; Code:

;; Make emacs startup faster
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun startup/revert-file-name-handler-alist ()
  "Revert file name handler alist."
  (setq file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;;; For performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Initialize melpa repo
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap straight.el
(setq straight-base-dir (expand-file-name "var" user-emacs-directory))
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
  (straight-check-for-modifications nil))

;; Load org for tangling
(straight-use-package 'org)

;; Tangle base configuration
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; Tangle user configuration
(let ((userconfig-location (expand-file-name "userconfig.org" user-emacs-directory)))
  (when (file-exists-p userconfig-location)
	(org-babel-load-file userconfig-location)))

;; Garbage collect
(garbage-collect)

;; Restore original GC values
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold gc-cons-threshold-original)
			(setq gc-cons-percentage gc-cons-percentage-original)))

;;; init.el ends here
