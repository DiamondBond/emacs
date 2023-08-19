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

(provide 'exwm-apps)
;;; exwm-apps.el ends here
