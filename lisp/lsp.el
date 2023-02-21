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
		  typescript-mode 	; ts-ls (tsserver wrapper)
		  python-mode     	; pyright
		  rust-mode       	; rust-analyzer
		  ruby-mode       	; solargraph
		  web-mode        	; ts-ls/HTML/CSS
		  clojure-mode		; clojure
		  clojurescript-mode	; clojurescript
		  clojurec-mode		; clojurec
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
  ;;(setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq completion-styles '(orderless)
		completion-category-defaults nil)
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

(use-package lsp-pyright
  :straight t
  :after lsp
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :init (setq lsp-pyright-python-executable-cmd "python3"))

;; LSP Tailwind-CSS
;; (straight-use-package
;;  '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

;; (use-package lsp-bridge
;;   :straight (:type git :host github :repo "manateelazycat/lsp-bridge" :branch "master")
;;   :files (:defaults ".py .tsx .js .cpp .c" "langserver" "acm")
;;   :init
;;   (global-lsp-bridge-mode))

;; YaSnippet
;; (use-package yasnippet
;;   :straight t
;;   :diminish yas-minor-mode
;;   :config
;;   ;;(setq yas-snippet-dirs '("~/emacs.d/snippets/"))
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :defer 4
;;   :straight t)

;; (use-package auto-yasnippet
;;   :disabled t)

;; Eglot
;; (use-package eglot
;;   :disabled t
;;   :config
;;   (setq read-process-output-max (* 1024 1024))
;;   (push :documentHighlightProvider eglot-ignored-server-capabilities)
;;   ;; Enable LSP support by default in programming buffers
;;   (add-hook 'prog-mode-hook #'eglot-ensure))

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

;; C#
(use-package csharp-mode
  :disabled t
  :defer 5)

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

(use-package rustic
  :disabled t
  :config
  (setq rustic-format-on-save nil))

;; Elisp
(use-package elisp-autofmt
  :straight t
  :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; CL
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-quicklisp)))

;; Scheme
(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(mit guile))
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  (setq geiser-default-implementation 'mit)
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-to-list 'auto-mode-alist
			   '("\\.sls\\'" . scheme-mode)
			   '("\\.sc\\'" . scheme-mode)))

;; MIT Scheme
(use-package geiser-mit
  :straight t
  :after geiser)

(defun geiser-save ()
  "Save geiser repl contents to input ring."
  (interactive)
  (geiser-repl--write-input-ring))

;; SICP
(use-package sicp
  :straight t)

;; GOAL
(define-derived-mode goal-mode lisp-mode
  "GOAL")
;; make gc files use our derived mode
(add-to-list 'auto-mode-alist '("\\.gc?\\'" . goal-mode))
;; run setup-goal when we enter lisp mode
(add-hook 'goal-mode-hook 'setup-goal)
(defun setup-goal ()
  ;; if we are in a gc file, change indent settings for GOAL
  (when (and (stringp buffer-file-name)
			 (string-match "\\.gc\\'" buffer-file-name))
	(put 'with-pp      'common-lisp-indent-function 0)
	(put 'while        'common-lisp-indent-function 1)
	(put 'rlet         'common-lisp-indent-function 1)
	(put 'until        'common-lisp-indent-function 1)
	(put 'countdown    'common-lisp-indent-function 1)
	(put 'defun-debug  'common-lisp-indent-function 2)
	(put 'defenum      'common-lisp-indent-function 2)

	;; disable slime
	(slime-mode -1)

	;; indent for common lisp, this makes if's look nicer
	(custom-set-variables '(lisp-indent-function 'common-lisp-indent-function))
	(autoload 'common-lisp-indent-function "cl-indent" "Common Lisp indent.")
	;; use spaces, not tabs
	(setq-default indent-tabs-mode nil)))

;; JSON
(use-package json-mode
  :straight t
  :mode ("\\.json\\'" . json-mode))

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

;; Python
(use-package python-mode
  :straight t
  :config
  (setq python-indent-offset standard-indent)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "/usr/bin/python3")
  (setq exec-path (cons "~/.local/bin" exec-path)))

;; Python env management
(use-package pyvenv
  :straight t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; Python formatter
(use-package python-black
  :straight t
  :after python)

;; Wolfram Mathematica
;; (use-package xah-wolfram-mode
;;   :straight (:type git :host github :repo "xahlee/xah-wolfram-mode" :branch "master"))

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
;;:mode (("\\.js\\'" . js2-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (subword-mode 1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save))
  (eldoc-mode +1)
  (diminish 'subword-mode "")
  (tide-hl-identifier-mode +1))

;; React JS
(use-package rjsx-mode
  :disabled t)

;; Tree-sitter
(use-package tree-sitter
  :straight t
  :diminish (tree-sitter-mode)
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; toggle tree-sitter-debug-mode with `C-c t d`
  (global-set-key (kbd "C-c t d") 'tree-sitter-debug-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; TypeScript
(use-package typescript-mode
  :after tree-sitter
  :straight t
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
	"TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  (add-hook 'typescriptreact-mode-hook #'setup-tide-mode))

;; load ts-repl with ts-mode
;;(with-eval-after-load "typescript-mode"
;;(require 'ts-repl)))
;;(flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
;; :mode (("\\.ts\\'" . typescript-mode)
;; 		 ("\\.tsx\\'" . typescript-mode)))

;; TypeScript IDE
(use-package tide
  :straight t
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition))

;; Add node_modules to PATH
(use-package add-node-modules-path
  :straight t
  :hook ((typescript-mode . add-node-modules-path)))

;; Prettier formatter
(use-package prettier-js
  :after (typescript-mode)
  :straight t)
;; :config
;; (add-hook 'web-mode-hook #'(lambda ()
;; 							   (enable-minor-mode
;; 								'("\\.jsx?\\'" . prettier-js-mode))
;; 							   (enable-minor-mode
;; 								'("\\.tsx?\\'" . prettier-js-mode)))))

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
		 ;;("\\.jsx\\'" .  web-mode)
		 ;;("\\.ts\\'" . web-mode)
		 ;;("\\.tsx\\'" . web-mode)
		 ("\\.html\\'" . web-mode))
  :commands web-mode)

;; Java
(use-package lsp-java
  :disabled t
  :init
  (setq lsp-java-vmargs
		(list
		 "-noverify"
		 "-Xmx3G"
		 "-XX:+UseG1GC"
		 "-XX:+UseStringDeduplication"
		 )

		;; Don't organise imports on save
		lsp-java-save-action-organize-imports nil

		;; Fetch less results from the Eclipse server
		lsp-java-completion-max-results 30

		;; Download 3rd party sources from Maven repo
		lsp-java-maven-download-sources t

		;; Don't format my source code (I use Maven for enforcing my
		;; coding style)
		lsp-java-format-enabled nil)
  :config
  (add-hook 'java-mode-hook #'lsp))

;; Debug Adapter Protocol
(use-package dap-mode
  :disabled t
  ;;:after lsp-mode
  :config
  (dap-auto-configure-mode)
  (setq dap-ui-locals-expand-depth 3)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

;; Clojure
(use-package clojure-mode
  :defer 3
  :straight t)

(use-package cider
  :straight t
  :defer 3
  :config
  (setq cider-repl-result-prefix "λ"
		cider-eval-result-prefix ""
		cider-connection-message-fn nil ; cute, but no!
		cider-use-overlays nil ; echo area is fine
		cider-repl-display-help-banner nil))

;;; GNU Assembly (GAS)
;; asm-mode is a built-in, but it has a terrible,
;; terrible comment command that must be rebound.
(use-package asm-mode
  :straight t
  ;; :pin manual
  ;; :ensure nil
  :bind
  (:map asm-mode-map
		(";" . nil)
		(":" . self-insert-command))
  :mode
  ("\\.s\\|.S\\'" . asm-mode))
;; The above regexp matches .emacs, we have to explicitly handle it.
;; Add .emacs to auto-mode-alist.
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))

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

(provide 'lsp)
;;; lsp.el ends here
