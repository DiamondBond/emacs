(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :config
  ;; Define backends for Llama and Dolphin models
  (setq-default
   gptel-backend-llama (gptel-make-ollama
						   "Ollama"
						 :host "localhost:11434"
						 :models '("llama3.1:latest")
						 :stream t)
   gptel-backend-codeqwen (gptel-make-ollama
						   "Codeqwen"
						 :host "localhost:11434"
						 :models '("codeqwen:latest")
						 :stream t)
   gptel-backend-deepseek-v2 (gptel-make-ollama
						   "Deepseek v2"
						 :host "localhost:11434"
						 :models '("deepseek-v2:16b")
						 :stream t)
   gptel-backend-deepseek-coder-v2 (gptel-make-ollama
						   "Deepseek Coder v2"
						 :host "localhost:11434"
						 :models '("deepseek-coder-v2:latest")
						 :stream t)
   gptel-backend-dolphin (gptel-make-ollama
							 "Dolphin"
						   :host "localhost:11434"
						   :models '("dolphin-llama3:8b")
						   :stream t))

  ;; Set Directives
  (setq gptel-directives
		'((default . "You are a large language model and a helpful assistant. Respond concisely.")
		  (reflect . "You are a world-class AI system, capable of complex reasoning and reflection. Reason through the query inside <thinking> tags, and then provide your final response inside <output> tags. If you detect that you made a mistake in your reasoning at any point, correct yourself inside <reflection> tags.")
		  (emacs . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
		  (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
		  (writing . "You are a large language model and a writing assistant. Respond concisely.")
		  (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  ;; Function to switch between models interactively
  (defun gptel-set-model (model)
    "Set the gptel model to MODEL interactively."
    (interactive
     (list (completing-read "Choose model: " '("llama" "codeqwen" "deepseek-v2" "deepseek-coder-v2" "dolphin"))))
    (setq gptel-backend
          (cond
           ((string= model "llama") gptel-backend-llama)
           ((string= model "codeqwen") gptel-backend-codeqwen)
           ((string= model "deepseek-v2") gptel-backend-deepseek-v2)
           ((string= model "deepseek-coder-v2") gptel-backend-deepseek-coder-v2)
           ((string= model "dolphin") gptel-backend-dolphin)
           (t (error "Unknown model: %s" model))))
    (setq gptel-model model)
    (message "GPTel model set to: %s" model))

  ;; Default to Llama model
  (gptel-set-model "llama")

  ;; Add post-response hook
  (add-hook 'gptel-post-response-hook 'gptel-end-of-response)

  ;; Define a keybinding for quickly switching models
  (global-set-key (kbd "C-c g M") 'gptel-set-model)
  (global-set-key (kbd "C-c g m") 'gptel-menu)

  ;; Optionally, you can define keybindings for quick model switching
  (defun gptel-use-llama ()
    "Quickly switch to Llama model."
    (interactive)
    (gptel-set-model "llama"))

  (defun gptel-use-codeqwen ()
    "Quickly switch to Codeqwen model."
    (interactive)
    (gptel-set-model "codeqwen"))

  (defun gptel-use-deepseek ()
    "Quickly switch to Deepseek model."
    (interactive)
    (gptel-set-model "deepseek-v2"))

  (defun gptel-use-deepseek-coder ()
    "Quickly switch to Deepseek Coder model."
    (interactive)
    (gptel-set-model "deepseek-coder-v2"))

  (defun gptel-use-dolphin ()
    "Quickly switch to Dolphin model."
    (interactive)
    (gptel-set-model "dolphin"))

  (global-set-key (kbd "C-c g c") 'gptel)
  (global-set-key (kbd "C-c g l") 'gptel-use-llama)
  (global-set-key (kbd "C-c g q") 'gptel-use-codeqwen)
  (global-set-key (kbd "C-c g d 1") 'gptel-use-deepseek)
  (global-set-key (kbd "C-c g d 2") 'gptel-use-deepseek-coder)
  (global-set-key (kbd "C-c g D") 'gptel-use-dolphin)

  (setq gptel-prompt-prefix-alist '()))

(provide 'gpt)
;;; gpt.el ends here
