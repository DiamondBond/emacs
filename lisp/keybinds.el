(define-prefix-command 'z-map)
(global-set-key (kbd "C-1") 'z-map)

;;
;; PRIVATE
;;

;; general
(define-key z-map (kbd "a") 'org-agenda)
(define-key z-map (kbd "f") 'find-file-other-frame)
(define-key z-map (kbd "g") 'golden-ratio)
(define-key z-map (kbd "o") 'olivetti-mode)
(define-key z-map (kbd "r") 'recentf-edit-list)
(define-key z-map (kbd "m") 'magit-status)
(define-key z-map (kbd "w") 'eww)
(define-key z-map (kbd "k") 'tab-bar-toggle)
(define-key z-map (kbd "l") 'minimap/toggle)
(define-key z-map (kbd "h") 'open-treemacs)
(define-key z-map (kbd "0") 'config/vscode-kill)
(define-key z-map (kbd "1") 'config/vscode-mode)
(define-key z-map (kbd "2") 'make-frame-command)

;; os-specific
(if (eq system-type 'gnu/linux)
	(define-key z-map (kbd "v") 'vterm))
(if (eq system-type 'windows-nt)
	(define-key z-map (kbd "v") 'eshell))

;; modeline
(define-key z-map (kbd "B") 'display-battery-mode)
(define-key z-map (kbd "T") 'display-time-mode)

;; functions
(define-key z-map (kbd "M") 'mu4e)
(define-key z-map (kbd "D") 'dashboard-refresh-buffer)
;; (define-key z-map (kbd "D") 'scratch-buffer)
(define-key z-map (kbd "L") 'minimap/refresh)
(define-key z-map (kbd "*") 'quick-calc)
(define-key z-map (kbd "O") 'org-redisplay-inline-images)
(define-key z-map (kbd "G") 'org-mark-ring-goto)
(define-key z-map (kbd "H") 'global-hl-line-mode)
(define-key z-map (kbd "s") 'ispell-word)
(define-key z-map (kbd "W") 'elfeed)
(define-key z-map (kbd "F") 'follow-mode)
(define-key z-map (kbd "U") 'undo-redo)
(define-key z-map (kbd "i") 'consult-imenu)
(define-key z-map (kbd "p") 'prettier-js)

;; quick
(define-key z-map (kbd "x") 'switch-to-buffer-other-frame)
(define-key z-map (kbd "k") 'compile)
(define-key z-map (kbd "e") 'eval-region)
(define-key z-map (kbd "b") 'browse-url)

;; auxiliary
(define-key z-map (kbd "S") 'speedbar-frame-mode)
(define-key z-map (kbd "2") 'consult-buffer-other-frame)
(define-key z-map (kbd "C-c") 'calendar)
(define-key z-map (kbd "C-d") 'dired-other-frame)
(define-key z-map (kbd "C-1") 'display-buffer-other-frame)

;; calendar
(define-key z-map (kbd ".") 'org-date-from-calendar)

;; files
(define-key z-map (kbd "n") 'notes-edit)
(define-key z-map (kbd "c") 'init-edit)
(define-key z-map (kbd "I") 'inbox-edit)
(define-key z-map (kbd "t") 'tasks-edit)

;;
;; GLOBAL
;;

;; function
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'menu-bar-mode)
(global-set-key (kbd "<f7>") 'scroll-bar-mode)
(global-set-key (kbd "<f8>") 'tool-bar-mode)
(global-set-key (kbd "<f10>") 'compile)
;; (global-set-key (kbd "S-<f5>") 'config/toggle-theme)
;; (global-set-key (kbd "S-<f5>") 'open-treemacs)
(global-set-key (kbd "S-<f7>") 'local-scroll-bar-toggle)
(global-set-key (kbd "S-<f8>") 'other-frame)
(global-set-key (kbd "<f9>") 'tab-bar-toggle)
(global-set-key (kbd "S-<f9>") 'toggle-frame-tab-bar)
(global-set-key (kbd "S-<f12>") 'display-line-numbers-mode)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "C-`") 'vterm-toggle)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-S-b") 'open-treemacs)
(global-set-key (kbd "C-S-m") 'minimap/toggle)
(global-set-key (kbd "C-c t t") 'open-treemacs)
(global-set-key (kbd "C-c t m") 'minimap/toggle)
(global-set-key (kbd "C-S-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-S-i") 'prettier-js)

;; windows
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "C-x W") 'elfeed-update)
(global-set-key (kbd "C-x x") 'window-swap-states)
(global-set-key (kbd "<s-C-return>") 'eshell-other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer-list-buffers)

;; window resizing
(global-set-key (kbd "s-C-<up>") 'enlarge-window)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)

;; hjkl
(global-set-key (kbd "s-C-k") 'enlarge-window)
(global-set-key (kbd "s-C-j") 'shrink-window)
(global-set-key (kbd "s-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-l") 'enlarge-window-horizontally)

;; windmove
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "s-K") 'windmove-swap-states-up)
(global-set-key (kbd "s-J") 'windmove-swap-states-down)
(global-set-key (kbd "s-H") 'windmove-swap-states-left)
(global-set-key (kbd "s-L") 'windmove-swap-states-right)

(global-set-key (kbd "M-<kp-begin>") 'execute-extended-command)
(global-set-key (kbd "M-<kp-up>") 'windmove-up)
(global-set-key (kbd "M-<kp-down>") 'windmove-down)
(global-set-key (kbd "M-<kp-left>") 'windmove-left)
(global-set-key (kbd "M-<kp-right>") 'windmove-right)

(global-set-key (kbd "M-S-<kp-up>") 'windmove-swap-states-up)
(global-set-key (kbd "M-S-<kp-down>") 'windmove-swap-states-down)
(global-set-key (kbd "M-S-<kp-left>") 'windmove-swap-states-left)
(global-set-key (kbd "M-S-<kp-right>") 'windmove-swap-states-right)

;; next/prev
;; (global-set-key (kbd "C-<tab>") 'next-buffer)
;; (global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
;;(global-set-key (kbd "C-<tab>") 'tab-next)
(define-key global-map (kbd "C-S-n") #'next-15-lines)
(define-key global-map (kbd "C-S-p") #'previous-15-lines)

;; mouse
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

;; indent/de-indent selection by one tab length
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;; kill word without copying it to your clipboard
(global-set-key (kbd "M-DEL") 'sanemacs/backward-kill-word)
(global-set-key (kbd "C-DEL") 'sanemacs/backward-kill-word)

(provide 'keybinds)
;;; keybinds.el ends here
