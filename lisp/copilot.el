(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  ;; (add-hook 'tree-sitter-mode-hook 'copilot-mode)
  (add-hook 'typescript-mode-hook 'copilot-mode)
  (add-hook 'typescriptreact-mode-hook 'copilot-mode)
  (add-hook 'c-mode-common-hook 'copilot-mode)
  (add-hook 'go-mode-hook 'copilot-mode)
  (add-hook 'python-mode-hook 'copilot-mode)
  (add-hook 'js-mode-hook 'copilot-mode)
  (add-hook 'js2-mode-hook 'copilot-mode)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (defun my/copilot-tab ()
	(interactive)
	(or (copilot-accept-completion)
		(indent-for-tab-command)))
  (with-eval-after-load 'copilot
	(evil-define-key 'insert copilot-mode-map
	  (kbd "<tab>") #'my/copilot-tab))
  )

(provide 'copilot)
;;; copilot.el ends here
