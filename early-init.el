;;; early-init.el --- Early Initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Diamond Bond
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Diamond Bond <diamondbond1@gmail.com>
;; URL: https://github.com/diamondbond/emacs
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This file provides the early-bootstrap configuration.
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Garbage Collection
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
(setq gc-cons-percentage-original gc-cons-percentage
	  gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

;; Silence compiler warnings
(setq warning-minimum-level :emergency)
(setq warning-suppress-types '((comp)))
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local cl-functions))

;; Warning: with 3, the compiler is free to perform dangerous optimizations.
(setq-default native-comp-speed 3) ;; -O3

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)
(setq native-comp-deferred-compilation-deny-list nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

;; Fully redraw the display before processing queued input events
(setq redisplay-dont-pause t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
	  menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Allow resizing the Emacs frame by individual pixels.
(setq frame-resize-pixelwise t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Update Emacs ui more frequently
(setq idle-update-delay 0.1)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
			  bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; MISC OPTIMIZATIONS ----
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

;;; early-init.el ends here
