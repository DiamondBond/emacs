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
   gptel-backend-dolphin (gptel-make-ollama
                          "Dolphin"
                          :host "localhost:11434"
                          :models '("dolphin-llama3:8b")
                          :stream t))

  ;; Function to switch between models interactively
  (defun gptel-set-model (model)
    "Set the gptel model to MODEL interactively."
    (interactive
     (list (completing-read "Choose model: " '("llama" "dolphin"))))
    (setq gptel-backend
          (cond
           ((string= model "llama") gptel-backend-llama)
           ((string= model "dolphin") gptel-backend-dolphin)
           (t (error "Unknown model: %s" model))))
    (setq gptel-model model)
    (message "GPTel model set to: %s" model))

  ;; Default to Llama model
  (gptel-set-model "llama")

  ;; Add post-response hook
  (add-hook 'gptel-post-response-hook 'gptel-end-of-response)

  ;; Define a keybinding for quickly switching models
  (global-set-key (kbd "C-c g m") 'gptel-set-model)

  ;; Optionally, you can define keybindings for quick model switching
  (defun gptel-use-llama ()
    "Quickly switch to Llama model."
    (interactive)
    (gptel-set-model "llama"))

  (defun gptel-use-dolphin ()
    "Quickly switch to Dolphin model."
    (interactive)
    (gptel-set-model "dolphin"))

  (global-set-key (kbd "C-c g l") 'gptel-use-llama)
  (global-set-key (kbd "C-c g d") 'gptel-use-dolphin)

  (setq gptel-prompt-prefix-alist '()))

(provide 'gpt)
;;; gpt.el ends here
