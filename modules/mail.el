;;; mail.el --- Mail Configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Diamond Bond
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Diamond Bond <diamondbond1@gmail.com>
;; URL: https://github.com/diamondbond/emacs
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This file provides the user configuration.

;;; Code:

(defun mu-setup/build-mu-binary ()
  "Compiles 'mu' binary."
  (interactive)
  (async-shell-command "cd ~/.emacs.d/straight/repos/mu; ./autogen.sh; ninja -C build"))

(defun mu-setup/init-mu ()
  "Initialize 'mu' db."
  (interactive)
  (async-shell-command "~/.emacs.d/straight/repos/mu/build/mu/mu init --maildir=/home/diamond/mail/ --my-address=diamondbond1@gmail.com"))

(defun mu-setup/rebuild-mu-index ()
  "Rebuilds 'mu' index."
  (interactive)
  (async-shell-command "~/.emacs.d/straight/repos/mu/build/mu/mu index"))

(defun mu-setup/automagic ()
  "Auto-magically configures 'mu'."
  (interactive)
  (mu-setup/build-mu-binary)
  (mu-setup/init-mu)
  (mu-setup/rebuild-mu-index))

(use-package mu4e
  :straight ( :host github
			  :repo "djcb/mu"
			  :branch "master"
			  :files ("build/mu4e/*")
			  :pre-build (("./autogen.sh") ("make")))
  :custom   (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  :config
  ;; default
  (require 'org-mu4e)
  (setq mu4e-maildir (expand-file-name "~/mail"))

  ;; set folders
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")

  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; view in browser
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
		'(("/INBOX"             . ?i)
		  ("/[Gmail].Sent Mail" . ?s)
		  ("/[Gmail].Trash"     . ?t)))

  ;; inbox-query
  (setq db/mu4e-inbox-query
		"(maildir:/Inbox OR maildir:/INBOX) AND flag:unread")

  ;; go-to-inbox function
  (defun db/go-to-inbox ()
	(interactive)
	(mu4e-headers-search db/mu4e-inbox-query))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; why would I want to leave my message open after I've sent it?
  (setq message-kill-buffer-on-exit t)
  ;; don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; don't ask to quit
  (setq mu4e-confirm-quit nil)

  ;; start mu4e
  (mu4e t))

(use-package mu4e-alert
  :straight t
  :after mu4e
  :config
  ;; show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query db/mu4e-inbox-query)

  ;; show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  ;; auto-enable notifications when opening mu4e
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  :init
  ;; enable notifications
  (mu4e-alert-enable-notifications))

(use-package smtpmail
  :straight t
  :config
  (setq message-send-mail-function 'smtpmail-send-it
		starttls-use-gnutls t
		smtpmail-starttls-credentials
		'(("smtp.gmail.com" 587 nil nil))
		smtpmail-auth-credentials
		(expand-file-name "~/.authinfo.gpg")
		smtpmail-default-smtp-server "smtp.gmail.com"
		smtpmail-smtp-server "smtp.gmail.com"
		smtpmail-smtp-service 587
		smtpmail-debug-info t))

(provide 'mail)
;;; mail.el ends here
