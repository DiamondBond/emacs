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
  (c-ts-indent-offset tab-width)
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
  :custom (go-ts-indent-offset 2)
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
  :hook ((typescript-ts-base-mode . add-node-modules-path)))

;; Prettier formatter
(use-package prettier-js
  :after (typescript-ts-mode))

;; Web mode (HTML)
(use-package web-mode
  :mode "\\.html\\'"
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-block-padding 2)
  (web-mode-style-padding 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t))

;; Python formatter
(use-package python-black
  :after python)

;; Lua
(use-package lua-ts-mode
  :straight (:type built-in)
  :custom (lua-ts-indent-offset 2))

;; Elisp
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :custom (markdown-fontify-code-blocks-natively t))

;; CSV
(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))

;; Netwide Assembly (NASM)
;; https://nasm.us/
(use-package nasm-mode
  :bind
  (:map nasm-mode-map
		(";" . self-insert-command))
  :mode
  ("\\.nasm\\|.asm\\'" . nasm-mode))

;; EBNF -- Extended Backus-Naur Form
;; https://github.com/nverno/ebnf-mode
(use-package ebnf-mode
  :mode
  ("\\.bnf\\'" . ebnf-mode))

(provide 'languages)
;;; languages.el ends here
