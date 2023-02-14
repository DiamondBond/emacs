(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
  :preface
  (defun chatgpt-session-expired ()
	"Fix expired login."
	(interactive)
	(async-shell-command "pkill ms-playwright/firefox")
	(run-in-vterm "chatgpt install"))
  :config
  (setq chatgpt-query-format-string-map '(
										  ;; ChatGPT.el defaults
										  ("nil" . "%s")
										  ("doc" . "Please write the documentation for the following function.\n\n%s")
										  ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
										  ("understand" . "What does the following function do?\n\n%s")
										  ("improve" . "Please improve the following code.\n\n%s")))
  :bind ("C-c q" . chatgpt-query))

(provide 'cgpt)
;;; cgpt.el ends here
