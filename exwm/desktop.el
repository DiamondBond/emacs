(defun run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
	(apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; EXWM Init
(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch 1)

  ;; Show battery status in the mode line
  (when (string= (system-name) "matebook")
	(progn
	  (display-battery-mode 1)))

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)

  ;; Alt-Tab setup
  (defvar exwm-workspace-previous-index nil "The previous active workspace index.")
  (advice-add 'exwm-workspace-switch :before #'exwm-workspace--current-to-previous-index)

  ;; Launch apps that will run in the background
  (run-in-background "nm-applet")
  (when (string= (system-name) "matebook")
	(progn
	  (run-in-background "pasystray")
	  (run-in-background "blueman-applet")
	  (run-in-background "flatpak run com.dropbox.Client")))
  )

;; Alt-Tab functions
(defun exwm-workspace--current-to-previous-index (_x)
  (setq exwm-workspace-previous-index exwm-workspace-current-index))
(defun exwm-workspace-switch-to-previous ()
  (interactive)
  "Switch to the previous active workspace."
  (let ((index exwm-workspace-previous-index))
	(exwm-workspace-switch index)))

;; EXWM Functions
(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))


(defun efs/exwm-update-title ()
  (pcase exwm-class-name
	("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
	;;("Firefox" (exwm-workspace-move-window 2))
	("Nm-connection-editor" (exwm-floating-toggle-floating))
	("Blueman-manager" (exwm-floating-toggle-floating))
	;; ("gnome-calendar" (exwm-floating-toggle-floating))
	("gnome-calculator" (exwm-floating-toggle-floating))
	("Gnome-terminal" (exwm-floating-toggle-floating))
	("Gnome-system-monitor" (exwm-floating-toggle-floating))
	("Pavucontrol" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))
	("Transmission-gtk" (exwm-floating-toggle-floating))
	("TelegramDesktop" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))
	("org.gnome.Nautilus" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))
	("Image Lounge" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))
	;; ("nomacs" (exwm-floating-toggle-floating)
	;;  (exwm-layout-toggle-mode-line))
	("Gpick" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))
	("Cheese" (exwm-floating-toggle-floating))
	("mpv" (exwm-floating-toggle-floating)
	 (exwm-layout-toggle-mode-line))))

;; Misc Functions
(defun rofi-linux-app ()
  (interactive)
  (start-process-shell-command
   "rofi" nil  "rofi -show combi"))

(defun x11-fav-launcher ()
  (interactive)
  (start-process-shell-command
   "bash" nil  "bash -ic ~/bin/launcher.sh"))

(defun statf ()
  (interactive)
  (start-process-shell-command
   "bash" nil  "bash -ic ~/bin/statf"))

(defun window-center ()
  (interactive)
  (start-process-shell-command
   "bash" nil  "bash -ic ~/bin/windowcenter"))

;;** `counsel-linux-app'
(defvar counsel-linux-apps-alist nil
  "List of data located in /usr/share/applications.")

(defvar counsel-linux-apps-faulty nil
  "List of faulty data located in /usr/share/applications.")

(defun counsel-linux-apps-list ()
  (setq counsel-linux-apps-alist nil)
  (let ((files
		 (delete ".."
				 (delete "."
						 (append
						  (file-expand-wildcards "/var/lib/flatpak/exports/share/applications/*.desktop")
						  (file-expand-wildcards "/usr/share/applications/*.desktop")
						  (file-expand-wildcards "~/.local/share/applications/*.desktop"))))))
	(dolist (file (cl-set-difference files (append (mapcar 'car counsel-linux-apps-alist)
												   counsel-linux-apps-faulty)
									 :test 'equal))
	  (add-to-list 'counsel-linux-apps-alist file)))
  counsel-linux-apps-alist)

(defun counsel-linux-app-action (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT."
  (call-process-shell-command
   (format "gtk-launch %s" (file-name-nondirectory desktop-shortcut))))

(defun counsel-linux-app ()
  "Launch a Linux desktop application, similar to Alt-<F2>."
  (interactive)
  (let ((selected-app (completing-read "Run a command: " (counsel-linux-apps-list))))
	(counsel-linux-app-action selected-app)))

;; EXWM
(use-package exwm
  :straight t
  :diminish exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; Rebind CapsLock to Ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; NOTE: Uncomment the following two options if you want window buffers
  ;;       to be available on all workspaces!

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;;(setq exwm-workspace-minibuffer-position 'top)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (when (string= (system-name) "matebook")
	(progn
	  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 2160x1440 --pos 0x0 --rotate normal")
	  (start-process-shell-command "bash" nil "bash -ic ~/.emacs.d/exwm/disable-touchscreen.sh")))
  (when (string= (system-name) "nitro")
	(progn
	  (start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output eDP-1-1 --off --output DP-1-1 --off")))

  ;; Disable mouse accel
  (when (string= (system-name) "nitro")
	(progn
	  (start-process-shell-command "bash" nil "bash -ic ~/bin/kmaccel")))

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 18)
  (exwm-systemtray-enable)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  ;; (setq mouse-autoselect-window t
  ;; 		focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
		'(?\C-x
		  ?\C-u
		  ?\C-h
		  ?\C-1
		  ?\s-h
		  ?\s-j
		  ?\s-k
		  ?\s-l
		  ?\s-H
		  ?\s-J
		  ?\s-K
		  ?\s-L
		  ?\M-x
		  ?\M-`
		  ?\M-&
		  ?\M-:
		  ?\C-\M-j  ;; Buffer list
		  ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
		`(
		  ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
		  ([?\s-r] . exwm-reset)

		  ;; Toggle exwm-mode
		  ([?\s-z] . exwm-input-toggle-keyboard)

		  ;; Move between windows
		  ([s-left] . windmove-left)
		  ([s-right] . windmove-right)
		  ([s-up] . windmove-up)
		  ([s-down] . windmove-down)

		  ;; Launch applications via shell command
		  ([?\s-&] . (lambda (command)
					   (interactive (list (read-shell-command "$ ")))
					   (start-process-shell-command command nil command)))

		  ;; Switch workspace
		  ([?\s-w] . exwm-workspace-switch)
		  ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

		  ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
		  ,@(mapcar (lambda (i)
					  `(,(kbd (format "s-%d" i)) .
						(lambda ()
						  (interactive)
						  (exwm-workspace-switch-create ,i))))
					(number-sequence 0 9))))

  ;; App Search
  (exwm-input-set-key (kbd "s-x") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  ;; App Hotkeys
  (exwm-input-set-key (kbd "s-<return>") 'app/xterm)
  (exwm-input-set-key (kbd "s-W") 'app/firefox)
  (exwm-input-set-key (kbd "s-E") 'app/files)
  (exwm-input-set-key (kbd "s-P") 'app/pavucontrol)

  ;; Functions
  (exwm-input-set-key (kbd "s-f") 'statf)
  (exwm-input-set-key (kbd "s-X") 'app-launcher)
  ;; (exwm-input-set-key (kbd "s-X") 'x11-fav-launcher)
  (exwm-input-set-key (kbd "s-g") 'app/calculator)
  (exwm-input-set-key (kbd "s-<tab>") 'exwm-workspace-switch-to-previous)
  ;; (exwm-input-set-key (kbd "M-<tab>") 'exwm-workspace-switch-to-previous)
  (exwm-input-set-key (kbd "C-s-SPC") 'efs/read-desktop-notification)
  (exwm-input-set-key (kbd "M-`") 'exwm-floating-hide)

  ;; Start EXWM
  (exwm-enable))

;; DE
(use-package desktop-environment
  :straight t
  :after exwm
  :init
  (defvar desktop-environment-mode-map
	(let ((desktop-environment--keybindings
		   `(;; Brightness
			 (,(kbd "<XF86MonBrightnessUp>") . ,(function desktop-environment-brightness-increment))
			 (,(kbd "<XF86MonBrightnessDown>") . ,(function desktop-environment-brightness-decrement))
			 (,(kbd "S-<XF86MonBrightnessUp>") . ,(function desktop-environment-brightness-increment-slowly))
			 (,(kbd "S-<XF86MonBrightnessDown>") . ,(function desktop-environment-brightness-decrement-slowly))
			 ;; Volume
			 (,(kbd "<XF86AudioRaiseVolume>") . ,(function desktop-environment-volume-increment))
			 (,(kbd "<XF86AudioLowerVolume>") . ,(function desktop-environment-volume-decrement))
			 (,(kbd "S-<XF86AudioRaiseVolume>") . ,(function desktop-environment-volume-increment-slowly))
			 (,(kbd "S-<XF86AudioLowerVolume>") . ,(function desktop-environment-volume-decrement-slowly))
			 (,(kbd "<XF86AudioMute>") . ,(function desktop-environment-toggle-mute))
			 (,(kbd "<XF86AudioMicMute>") . ,(function desktop-environment-toggle-microphone-mute))
			 ;; Screenshot
			 (,(kbd "S-<print>") . ,(function desktop-environment-screenshot-part))
			 (,(kbd "<print>") . ,(function desktop-environment-screenshot))
			 ;; Replace s-l (default: screen-lock) with windmove-right
			 (,(kbd "s-l") . ,(function windmove-right))
			 ;; Screen locking
			 (,(kbd "s-<f2>") . ,(function desktop-environment-lock-screen))
			 (,(kbd "<XF86ScreenSaver>") . ,(function desktop-environment-lock-screen))
			 ;; Wifi controls
			 (,(kbd "<XF86WLAN>") . ,(function desktop-environment-toggle-wifi))
			 ;; Bluetooth controls
			 (,(kbd "<XF86Bluetooth>") . ,(function desktop-environment-toggle-bluetooth))
			 ;; Music controls
			 (,(kbd "<XF86AudioPlay>") . ,(function desktop-environment-toggle-music))
			 (,(kbd "<XF86AudioPrev>") . ,(function desktop-environment-music-previous))
			 (,(kbd "<XF86AudioNext>") . ,(function desktop-environment-music-next))
			 (,(kbd "<XF86AudioStop>") . ,(function desktop-environment-music-stop))))
		  (map (make-sparse-keymap)))
	  (dolist (keybinding desktop-environment--keybindings)
		(define-key map (car keybinding) (cdr keybinding)))
	  map)
	"Keymap for `desktop-environment-mode'.")
  :config
  (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; Notifications
(defun efs/read-desktop-notification ()
  "Dismiss desktop notification."
  (interactive)
  (start-process-shell-command "bash" nil "bash -ic ~/bin/dunst_dismiss"))

(defun efs/disable-desktop-notifications ()
  "Enable desktop notifications."
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun efs/enable-desktop-notifications ()
  "Disable desktop notifications."
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun efs/toggle-desktop-notifications ()
  "Toggle desktop notifications."
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

;; Apps
(defun app/files ()
  "Nautilus"
  (interactive)
  (start-process-shell-command "nautilus" nil "nautilus"))

(defun app/terminal ()
  "Gnome Terminal"
  (interactive)
  (start-process-shell-command "gnome-terminal" nil "gnome-terminal"))

(defun app/code ()
  "Visual Studio Code"
  (interactive)
  (start-process-shell-command "code" nil "code"))

(defun app/gsm ()
  "Gnome System Monitor"
  (interactive)
  (start-process-shell-command "gnome-system-monitor" nil "gnome-system-monitor"))

(defun app/xterm ()
  "XTerm"
  (interactive)
  (start-process-shell-command "xterm" nil "xterm"))

(defun app/calculator ()
  "Gnome Calculator"
  (interactive)
  (start-process-shell-command "gnome-calculator" nil "gnome-calculator"))

(defun app/calendar ()
  "Gnome Calendar"
  (interactive)
  (start-process-shell-command "gnome-calendar" nil "gnome-calendar"))

(defun app/pavucontrol ()
  "Pavucontrol"
  (interactive)
  (start-process-shell-command "pauvcontrol" nil "pavucontrol"))

(defun app/arandr ()
  "Arandr"
  (interactive)
  (start-process-shell-command "arandr" nil "arandr"))

(defun app/firefox ()
  "Firefox"
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(defun app/thunderbird ()
  "Thunderbird"
  (interactive)
  (start-process-shell-command "thunderbird" nil "thunderbird"))

(defun app/chromium ()
  "Chromium"
  (interactive)
  (start-process-shell-command "flatpak run" nil "flatpak run com.github.Eloston.UngoogledChromium"))

(defun app/telegram ()
  "Telegram"
  (interactive)
  (start-process-shell-command "flatpak run" nil "flatpak run org.telegram.desktop"))

(defun app/discord ()
  "Discord"
  (interactive)
  (start-process-shell-command "flatpak run" nil "flatpak run com.discordapp.Discord"))

;; Launcher
(defun app-launcher ()
  "Select and run an app function using `'completing-read`'."
  (interactive)
  (let ((app-functions '(app/files
						 app/terminal
						 app/code
						 app/gsm
						 app/xterm
						 app/calculator
						 app/calendar
						 app/pavucontrol
						 app/arandr
						 app/firefox
						 app/thunderbird
						 app/chromium
						 app/telegram
						 app/discord)))
	(funcall (intern (completing-read "Select an app: " app-functions nil t)))))

(provide 'desktop)
;;; desktop.el ends here
