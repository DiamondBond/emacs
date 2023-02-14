(use-package ejira
  :straight (:type git :host github :repo "nyyManni/ejira" :branch "master")
  :defer 2
  :init
  (setq jiralib2-url              "https://sallypos.atlassian.net"
		jiralib2-auth             'token
		jiralib2-user-login-name  "diamondbond1@gmail.com"

		;; NOTE, this directory needs to be in `org-agenda-files'`
		ejira-org-directory       "~/org/jira"
		ejira-projects            '("SP")

		ejira-priorities-alist    '(("Highest" . ?A)
									("High"    . ?B)
									("Medium"  . ?C)
									("Low"     . ?D)
									("Lowest"  . ?E))
		ejira-todo-states-alist   '(("To Do"       . 1)
									("In Progress" . 2)
									("CODE REVIEW" . 3)
									("Done"        . 4)))
  ;; Load jira token
  (when (file-readable-p "~/org/jira/jiralib2-token.el")
	(load-file "~/org/jira/jiralib2-token.el"))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
	 ((ejira-jql "resolution = unresolved and assignee = currentUser()"
				 ((org-agenda-overriding-header "Assigned to me")))))))

(use-package jira-markup-mode
  :straight t
  :defer 6)

(provide 'jira)
;;; jira.el ends here
