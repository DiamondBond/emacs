;;; -*- lexical-binding: t; -*-
;; LSP (built-in): diagnostics and go-to-definition, without UI chrome
(use-package eglot
  :straight (:type built-in)
  :hook ((c-ts-base-mode           ; clangd
		  go-ts-mode               ; gopls
		  rust-ts-mode             ; rust-analyzer
		  js-ts-mode               ; typescript-language-server
		  typescript-ts-base-mode  ; typescript-language-server
		  ) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-code-action-indications nil)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
									   :documentOnTypeFormattingProvider
									   :inlayHintProvider
									   :semanticTokensProvider)))

(defun db/eglot-format-buffer ()
  "Format the buffer with eglot when a language server manages it."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
	(eglot-format-buffer)))

(defun db/eglot-format-on-save ()
  "Format with the language server before saving."
  (add-hook 'before-save-hook #'db/eglot-format-buffer nil t))

;; Tree-sitter (built-in). Grammars install to ~/.emacs.d/tree-sitter/ on
;; first use. Languages not listed here keep their classic modes.
(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-font-lock-level 4)
  (treesit-enabled-modes '(c-ts-mode c++-ts-mode c-or-c++-ts-mode
									 go-ts-mode go-mod-ts-mode rust-ts-mode
									 js-ts-mode typescript-ts-mode tsx-ts-mode
									 json-ts-mode css-ts-mode python-ts-mode
									 lua-ts-mode ruby-ts-mode cmake-ts-mode))
  :bind ("C-c t d" . treesit-explore-mode))

;; C/C++
(use-package c-ts-mode
  :straight (:type built-in)
  :custom
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset tab-width)
  :hook (c-ts-base-mode . subword-mode))

;; astyle formatter function
(defun astyle-buffer (&optional justify)
  "Format buffer using astyle --style=kr."
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
	(shell-command-on-region
	 (point-min)
	 (point-max)
	 "astyle --style=kr"
	 nil
	 t)
	(goto-line saved-line-number)))

;; Go
(use-package go-ts-mode
  :straight (:type built-in)
  :custom (go-ts-mode-indent-offset 2)
  :config
  (defun db/go-mode-hook()
	(setq tab-width 2)
	(set (make-local-variable 'compile-command)
		 "go test"))
  :hook ((go-ts-mode . db/go-mode-hook)
		 (go-ts-mode . db/eglot-format-on-save)
		 (go-ts-mode . subword-mode)))

;; Rust
(use-package rust-ts-mode
  :straight (:type built-in)
  :hook (rust-ts-mode . db/eglot-format-on-save))

(use-package cargo
  :straight t
  :defer 3)

;; JavaScript
(use-package js
  :straight (:type built-in)
  :custom (js-indent-level 2)
  :hook (js-base-mode . subword-mode))

;; TypeScript (typescript-ts-base-mode covers both .ts and .tsx)
(add-hook 'typescript-ts-base-mode-hook #'subword-mode)

;; Add node_modules to PATH
(use-package add-node-modules-path
  :straight t
  :hook ((typescript-ts-base-mode . add-node-modules-path)))

;; Prettier formatter
(use-package prettier-js
  :after (typescript-ts-mode)
  :straight t)

;; Web mode (HTML)
(use-package web-mode
  :straight t
  :config
  (setq web-mode-content-types-alist '(("html" . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)

  ;; adjust web-mode indent
  (defun web-mode-init-hook ()
	"Hooks for Web mode.  Adjust indent."
	(setq web-mode-markup-indent-offset 4))
  (add-hook 'web-mode-hook 'web-mode-init-hook)
  :mode ("\\.html\\'" . web-mode)
  :commands web-mode)

;; Python formatter
(use-package python-black
  :straight t
  :after python)

;; Lua
(use-package lua-ts-mode
  :straight (:type built-in)
  :custom (lua-ts-indent-offset 2))

;; Elisp
(use-package elisp-autofmt
  :straight t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :custom (markdown-fontify-code-blocks-natively t))

;; CSV
(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode))

;; Netwide Assembly (NASM)
;; https://nasm.us/
(use-package nasm-mode
  :straight t
  :bind
  (:map nasm-mode-map
		(";" . self-insert-command))
  :mode
  ("\\.nasm\\|.asm\\'" . nasm-mode))

;; EBNF -- Extended Backus-Naur Form
;; https://github.com/nverno/ebnf-mode
(use-package ebnf-mode
  :straight t
  :mode
  ("\\.bnf\\'" . ebnf-mode))

(provide 'languages)
;;; languages.el ends here
