(server-start)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(defvar my/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun my/kill-panel ()
  "Stop any running Polybar processes"
  (interactive)
  (when my/polybar-process
    (ignore-errors
      (kill-process my/polybar-process)))
  (setq my/polybar-process nil))

(defun my/start-panel ()
  "Start Polybar"
  (interactive)
  (my/kill-panel)
  (setq my/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun my/send-polybar-hook (module-name hook-index)
  "Generic IPC hook function for communicating with Polybar"
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun my/send-polybar-exwm-ws-indicator ()
  "Wraps the hook for the `exwm-ws-indicator' Polybar module"
  (my/send-polybar-hook "exwm-ws-indicator" 1))

(defun my/polybar-exwm-ws-indicator ()
  "Switch case to select the appropriate indicator"
  (pcase exwm-workspace-current-index
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")))

(defun my/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale ~/guix-dotfiles/home/files/emacs/exwm/guix.png"))

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun my/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)
  ;; Start the Polybar panel
  (my/start-panel)
  ;; Launch apps that will run in the background
  ;; (my/run-in-background "polybar")
  )

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun my/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun my/exwm-configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("mpv"
     (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ;; ("Firefox" (exwm-workspace-move-window 2)))
  ))
  
(use-package exwm
  :config
  ;; Set default number of workspaces
  (setq exwm-workspace-number 5)
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)
  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'my/exwm-update-title)
  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'my/exwm-configure-window-by-class)
  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'my/exwm-init-hook)
  ;; Update panel indicator when workspace changes
  (add-hook 'exwm-workspace-switch-hook #'my/send-polybar-exwm-ws-indicator)
  ;; Set the screen resolution (update this to be the correct resolution for your screen)
  ;; (require 'exwm-randr)
  ;; (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")
  ;; Set the wallpaper after changing the resolution
  (my/set-wallpaper)
  ;; Set starting workspace to 1
  (setq exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))
  
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\M-&
	  ?\M-:))
  ;; Ctrl-q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  
  (setq exwm-input-global-keys
	`(;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-r] . exwm-reset)
	  ;; Move between windows
	  ([?\s-h] . windmove-left)
	  ([?\s-j] . windmove-down)
	  ([?\s-k] . windmove-up)
	  ([?\s-l] . windmove-right)
	  ;; Swap windows
	  ([?\s-H] . windmove-swap-states-left)
	  ([?\s-J] . windmove-swap-states-down)
	  ([?\s-K] . windmove-swap-states-up)
	  ([?\s-L] . windmove-swap-states-right)
	  ;; Toggle fullscreen
	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ;; Launch applications via shell comman
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
	  ;; Switch workspace
	  ([?\s-w] . exwm-workspace-switch)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) . (lambda ()
						     (interactive)
						     (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config
  (setq desktop-environment-update-exwm-global-keys :prefix)
  (define-key desktop-environment-mode-map (kbd "s-l") nil)
  (desktop-environment-mode))

(use-package vertico
  :init (vertico-mode))
