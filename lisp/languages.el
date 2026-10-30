;;; -*- lexical-binding: t; -*-
(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
		  '(orderless))) ;; Configure orderless
  :hook ((c-mode          	; clangd
		  c++-mode        	; clangd
		  c-or-c++-mode   	; clangd
		  js2-mode        	; ts-ls (tsserver wrapper)
		  js-mode         	; ts-ls (tsserver wrapper)
		  rjsx-mode       	; ts-ls (tsserver wrapper)
		  js-jsx-mode     	; ts-ls (tsserver wrapper)
		  typescript-ts-base-mode ; ts-ls (tsserver wrapper)
		  rust-mode       	; rust-analyzer
		  ruby-mode       	; solargraph
		  web-mode        	; ts-ls/HTML/CSS
		  ) . lsp-deferred)
  ((lsp-completion-mode . my/lsp-mode-setup-completion))
  ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-idle-delay 0.5)
  (setq lsp-clients-typescript-server "typescript-language-server"
		lsp-clients-typescript-server-args '("--stdio"))
  (setq lsp-disabled-clients '(eslint)))

(use-package lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

;; C/C++
(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
		 (lambda () (require 'ccls) (lsp))))

(use-package modern-cpp-font-lock
  :straight t
  :hook
  ((c++-mode) . #'modern-c++-font-lock-mode))

;;; cmake-font-lock
;; Better syntax highlighting for CMake scripts.
;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :defer t)

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
(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (defun db/go-mode-hook()
	(setq tab-width 2)
	(add-hook 'before-save-hook 'gofmt-before-save)
	(set (make-local-variable 'compile-command)
		 "go test"))
  :hook ((go-mode . lsp-deferred))
  :hook ((go-mode . db/go-mode-hook))
  :hook ((go-mode . subword-mode)))

;; Rust
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer 3)

;; Elisp
(use-package elisp-autofmt
  :straight t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; CSV
(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode))

;; LUA
(use-package lua-mode
  :straight t
  :config
  (setq lua-indent-level 2))

;; Ruby
(use-package ruby-mode
  :defer 4
  :straight t)

;; Python formatter
(use-package python-black
  :straight t
  :after python)

;; Markdown
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :hook ((markdown-mode . auto-fill-mode)))

;; FlyCheck
(use-package flycheck
  :straight t
  :bind (:map flycheck-mode-map
			  ("C-c n" . flycheck-next-error)
			  ("C-c p" . flycheck-previous-error)))
;; JavaScript
(use-package js-mode
  :straight (:type built-in)
  :init
  (with-eval-after-load 'subword
	(diminish 'subword-mode))
  (add-hook 'js-mode-hook 'subword-mode)
  :mode (("\\.js\\'" . js-mode)))

(use-package js2-mode
  :straight t
  :diminish (js2-mode js2-minor-mode)
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-show-strict-warnings nil)
  ;; override js2-error
  (custom-set-faces
   '(js2-error ((t (:foreground unspecified :weight normal)))))
  ;; just for JS linting
  (add-hook 'js-mode-hook 'js2-minor-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (subword-mode 1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save))
  (eldoc-mode +1)
  (diminish 'subword-mode "")
  (tide-hl-identifier-mode +1))

;; Tree-sitter (built-in). Grammars install to ~/.emacs.d/tree-sitter/ on
;; first use. Only these ts-modes are enabled; other languages keep their
;; classic modes so their hooks (lsp, ccls, gofmt...) keep firing.
;; .ts/.tsx open in typescript-ts-mode/tsx-ts-mode out of the box.
(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-font-lock-level 4)
  (treesit-enabled-modes '(typescript-ts-mode tsx-ts-mode json-ts-mode css-ts-mode))
  :bind ("C-c t d" . treesit-explore-mode))

;; TypeScript (typescript-ts-base-mode covers both .ts and .tsx)
(add-hook 'typescript-ts-base-mode-hook #'setup-tide-mode)

;; TypeScript IDE
(use-package tide
  :straight t
  :config
  (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition))

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
  (setq web-mode-content-types-alist '(;;("jsx" . "\\.[jt]sx?\\'")
									   ("html" . "\\.html\\'")))
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

  ;; disable other js linters
  (setq-default flycheck-disabled-checkers
				(append flycheck-disabled-checkers
						'(javascript-jshint json-jsonlist)))
  ;; enable eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :mode (;;("\\.js\\'" . web-mode)
		 ("\\.html\\'" . web-mode))
  :commands web-mode)

;;; Netwide Assembly (NASM)
;; https://nasm.us/
(use-package nasm-mode
  :straight t
  :bind
  (:map nasm-mode-map
		(";" . self-insert-command))
  :mode
  ("\\.nasm\\|.asm\\'" . nasm-mode))

;;; EBNF -- Extended Backus-Naur Form
;; https://github.com/nverno/ebnf-mode
(use-package ebnf-mode
  :straight t
  :mode
  ("\\.bnf\\'" . ebnf-mode))

(provide 'languages)
;;; languages.el ends here
