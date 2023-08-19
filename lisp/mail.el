(defun mu-setup/build-mu-binary ()
  "Compiles 'mu' binary."
  (interactive)
  (async-shell-command "cd ~/.emacs.d/straight/repos/mu; ./autogen.sh; ninja -C build"))

(defun mu-setup/init-mu ()
  "Initialize 'mu' db."
  (interactive)
  (async-shell-command "mu init --maildir=/home/diamond/mail/ --my-address=diamondbond1@gmail.com"))

(defun mu-setup/rebuild-mu-index ()
  "Rebuilds 'mu' index."
  (interactive)
  (async-shell-command "mu index"))

(defun mu-setup/automagic ()
  "Auto-magically configures 'mu'."
  (interactive)
  ;; (mu-setup/build-mu-binary)
  ;; (sit-for 5)
  (mu-setup/init-mu)
  ;; (sit-for 5)
  (mu-setup/rebuild-mu-index))

(defun sync/mail ()
  "Sync email."
  (interactive)
  (async-shell-command "offlineimap")
  (mu4e-update-index))

(use-package mu4e
  :straight t
  :custom (mu4e-mu-binary "/usr/local/bin/mu")
  :diminish mu4e-headers-mode
  :diminish mu4e-modeline-mode
  :config
  (setq mu4e-maildir (expand-file-name "~/mail"))

  ;; set folders
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")
  ;; (setq mu4e-mu-home "~/.cache/mu")

  ;; don't save message to Sent Messages, GMail/IMAP will take care of this
  (setq mu4e-sent-messages-behavior 'delete)

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
  (setq mu4e-change-filenames-when-moving t)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
		'(("/INBOX"             . ?i)
		  ("/[Gmail].Sent Mail" . ?s)
		  ("/[Gmail].Trash"     . ?t)))

  ;; attachments go here
  (setq mu4e-attachment-dir  "~/mail/attachments")

  ;; modify behavior when putting something in the trash (T flag) so as
  ;; to make it sync to the remote server. This code deals with the bug
  ;; that, whenever a message is marked with the trash label T,
  ;; offlineimap wont sync it back to the gmail servers.
  ;;
  ;; NOTE: Taken from
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
	(if (zerop nth) (cdr list)
	  (let ((last (nthcdr (1- nth) list)))
		(setcdr last (cddr last))
		list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
			   '(trash
				 :char ("d" . "▼")
				 :prompt "dtrash"
				 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
				 :action (lambda (docid msg target)
						   (mu4e~proc-move docid
										   (mu4e~mark-check-target target) "-N"))))

  ;; inbox-query
  (setq db/mu4e-inbox-query
		"(maildir:/Inbox OR maildir:/INBOX) AND flag:unread")

  ;; go-to-inbox function
  (defun db/go-to-inbox ()
	"View unread inbox."
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

  ;; disable mu4e-modeline
  ;;(mu4e-modeline-mode -1)

  ;; disable mu4e modeline on system with newer mu4e package
  (when (string= (system-name) "nitro")
	(progn
	  (add-hook 'mu4e-main-mode-hook (lambda () (mu4e-modeline-mode -1)))
	  (add-hook 'dashboard-mode-hook (lambda () (mu4e-modeline-mode -1)))
	  (add-hook 'emacs-startup-hook (lambda () (mu4e-modeline-mode -1)))))

  ;; start mu4e
  (mu4e t))

(use-package mu4e-alert
  :disabled t
  :init
  (defun db/mu4e-notif ()
	"Display both mode line and desktop alerts for incoming new emails."
	(interactive)
	(mu4e-update-mail-and-index 1)        ; getting new emails is ran in the background
	(mu4e-alert-enable-mode-line-display) ; display new emails in mode-line
	(mu4e-alert-enable-notifications))    ; enable desktop notifications for new emails
  (defun db/mu4e-refresh ()
	"Refresh emails every 300 seconds and display desktop alerts."
	(interactive)
	(mu4e t)                            ; start silently mu4e (mandatory for mu>=1.3.8)
	(run-with-timer 0 300 'db/mu4e-notif))
  :after mu4e
  :bind ("<f2>" . db/mu4e-refresh)  ; F2 turns Emacs into a mail client
  :config

  ;; show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query db/mu4e-inbox-query)

  ;; show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  ;; mode line alerts:
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

  ;; desktop alerts
  (mu4e-alert-set-default-style 'libnotify)
  ;; auto-enable notifications when opening mu4e
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  ;; enable notifications
  (mu4e-alert-enable-notifications))

(use-package smtpmail
  :straight t
  :config
  (setq message-send-mail-function 'async-smtpmail-send-it
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
