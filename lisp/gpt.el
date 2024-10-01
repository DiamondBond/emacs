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
   gptel-backend-llama32 (gptel-make-ollama
						   "Ollama 32"
						 :host "localhost:11434"
						 :models '("llama3.2:latest")
						 :stream t)
   gptel-backend-mistral-nemo (gptel-make-ollama
						   "Mistral Nemo"
						 :host "localhost:11434"
						 :models '("mistral-nemo:latest")
						 :stream t)
   gptel-backend-codeqwen (gptel-make-ollama
						   "Codeqwen"
						 :host "localhost:11434"
						 :models '("codeqwen:latest")
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
		  (typescript . "You are an expert Typescript programmer. Provide code as output.")
		  (writing . "You are a large language model and a writing assistant. Respond concisely.")
		  (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  ;; Function to switch between models interactively
  (defun gptel-set-model (model)
    "Set the gptel model to MODEL interactively."
    (interactive
     (list (completing-read "Choose model: " '("llama" "llama32" "mistral-nemo" "codeqwen" "deepseek-coder-v2" "dolphin"))))
    (setq gptel-backend
          (cond
           ((string= model "llama") gptel-backend-llama)
           ((string= model "llama32") gptel-backend-llama32)
           ((string= model "mistral-nemo") gptel-backend-mistral-nemo)
           ((string= model "codeqwen") gptel-backend-codeqwen)
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

  (defun gptel-use-llama32 ()
    "Quickly switch to Llama 3.2 model."
    (interactive)
    (gptel-set-model "llama32"))

  (defun gptel-use-mistral ()
    "Quickly switch to Mistral model."
    (interactive)
    (gptel-set-model "mistral-nemo"))

  (defun gptel-use-codeqwen ()
    "Quickly switch to Codeqwen model."
    (interactive)
    (gptel-set-model "codeqwen"))

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
  (global-set-key (kbd "C-c g L") 'gptel-use-llama32)
  (global-set-key (kbd "C-c g q") 'gptel-use-codeqwen)
  (global-set-key (kbd "C-c g d") 'gptel-use-deepseek-coder)
  (global-set-key (kbd "C-c g n") 'gptel-use-mistral)
  (global-set-key (kbd "C-c g D") 'gptel-use-dolphin)

  (setq gptel-prompt-prefix-alist '()))

(provide 'gpt)
;;; gpt.el ends here
