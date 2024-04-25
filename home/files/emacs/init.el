;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.guix-home/profile/share/emacs/site-lisp")
(guix-emacs-autoload-packages)

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-enable-imenu-support t
	use-package-minimum-reported-time 0.01)
  (require 'use-package))

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by Emacs are placed.")

(defvar user-etc-directory
  (file-name-concat user-cache-directory "etc/")
  "The directory where packages place their configuration files.")

(defvar user-var-directory
  (file-name-concat user-cache-directory "var/")
  "The directory where packages place their persistent data files.")

(defun expand-etc-file-name (file)
  "Expand filename FILE relative to `user-etc-directory'."
  (file-name-concat user-etc-directory
		    (convert-standard-filename file)))

(defun expand-var-file-name (file)
  "Expand filename FILE relative to `user-var-directory'."
  (file-name-concat user-var-directory
		    (convert-standard-filename file)))

(defalias 'etc #'expand-etc-file-name)
(defalias 'var #'expand-var-file-name)

(use-package general
  :init
  (general-evil-setup)
  ;; Create SPC leader key, to be used in the macro.
  (general-create-definer global-definer
    :keymaps 'override
    :states '(normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  ;; Add a definer for each of the major modes
  (general-create-definer major-mode-definer
    :keymaps 'override
    :states '(normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key "localleader"))
  ;; Add an additional minor mode definer, for each of the modes.
  ;; It is key to remember that in this case, the :keymaps option refers to the
  ;; minor mode, not the keymap.
  (general-create-definer minor-mode-definer
    :keymaps 'override
    :definer 'minor-mode
    :states '(normal hybrid motion visual operator)
    :prefix ",")
  ;; Macro to define all key-pockets. It adapts to the name passed, and defines additonal macros to be
  ;; used to define keybindings. See `general-global-buffer' below.
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
      Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
	 :wrapping global-definer
	 :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
	 :infix ,infix-key
	 :wk-full-keys nil
	 "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
	,@body)))
  :config
  (+general-global-menu! "goto" "g")
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "search" "s")
  (+general-global-menu! "term" "t")
  (+general-global-menu! "vc" "v")
  (+general-global-menu! "toggle" "x"))

(use-package evil
  :general-config
  (general-nmap
    ;; for some reason `xref-find-definitions' and by
    ;; consequence `evil-goto-definition' stops working randomly
    ;; for some elisp symbols. `embark-dwim' doesn't for
    ;; some reason?
    ;; "gd" 'embark-dwim
    "gd" 'xref-find-definitions
    "C-S-t" 'xref-go-forward)
  (general-nvmap
    "L" 'evil-end-of-line
    "H" 'my/back-to-indentation-or-beginning)
  (general-omap
    "L" 'evil-end-of-line
    "H" 'my/back-to-indentation-or-beginning)
  :custom
  (evil-want-keybinding nil)
  (evil-symbol-word-search t)
  (evil-echo-state nil)
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-respect-visual-line-mode t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :preface
  (defun my/back-to-indentation-or-beginning ()
    (interactive)
    (if (= (point) (progn (beginning-of-line-text) (point)))
	(beginning-of-line)))
  :init (evil-mode))

(use-package evil-collection
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  :init (evil-collection-init))

(use-package anzu
  :custom (anzu-search-threshold 10000)
  :init (global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-surround
  :general-config (general-vmap "s" 'evil-surround-region)
  :init (global-evil-surround-mode))

(use-package modus-themes
  :init
  (defun my/modus-themes-custom-faces ())
  ;; Using the hook lets our changes persist when we use the command
  ;; `modus-themes-toggle', `modus-themes-select', and `modus-themes-load-random'.
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces))

(use-package ef-themes
  :init
  (defun my/ef-themes-custom-faces ())
  ;; Using the hook lets our changes persist when we use the command
  ;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
  ;; TODO: use ef-dream when ef-themes v1.7 is released
  (ef-themes-select 'ef-dark))

(use-package rainbow-mode
  :hook ((prog-mode text-mode) . rainbow-mode)
  :general-config (+general-global-toggle
		    "c" 'rainbow-mode)
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil))

(use-package fontaine
  :general-config (+general-global-toggle
		    "f" 'fontaine-set-preset)
  :custom
  (x-underline-at-descent-line nil)
  (text-scale-remap-header-line t)
  (fontaine-latest-state-file (var "fontaine-latest-state.eld"))
  (fontaine-presets '((small
		       :default-family "Iosevka Comfy Motion"
		       :default-height 80
		       :variable-pitch-family "Iosevka Comfy Duo")
		      (regular) ; like this it uses all the fallback values and is named `regular'
		      (medium
		       :default-weight semilight
		       :default-height 115
		       :bold-weight extrabold)
		      (large
		       :inherit medium
		       :default-weight regular
		       :default-height 150)
		      (live-stream
		       :default-family "Iosevka Comfy Wide Motion"
		       :default-height 150
		       :default-weight medium
		       :fixed-pitch-family "Iosevka Comfy Wide Motion"
		       :variable-pitch-family "Iosevka Comfy Wide Duo"
		       :bold-weight extrabold)
		      (presentation
		       :default-height 180)
		      (t
		       :default-family "Iosevka Comfy"
		       :default-weight regular
		       :default-slant normal
		       :default-height 100
		       :fixed-pitch-family "Iosevka Comfy"
		       :variable-pitch-family "Iosevka Comfy Motion Duo")))
  :config
  ;; Set last preset of fallback to desired style from `fontaine-presets'
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'large))
  ;; TODO: use `fontaine-mode' when fontaine version 2.0 hits Guix upstream
  ;; The other side of `fontaine-restore-latest-preset'
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset)
  ;; Persist font configurations while switching themes. The
  ;; `enable-theme-functions' is from Emacs 29.
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom (hl-todo-wrap-movement t))

(use-package consult-todo
  :general-config (+general-global-goto
		    "t" 'consult-todo))

(use-package crux
  :general (general-nvmap "R" 'crux-duplicate-current-line-or-region))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package visual-regexp
  :general-config (global-definer "r" 'vr/query-replace))

(use-package vundo
  :general-config (global-definer "u" 'vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  (vundo-window-max-height 8))

(use-package undo-fu-session
  :custom (undo-fu-session-directory (var "undo-fu-session/"))
  :init (undo-fu-session-global-mode))

(use-package avy
  :general-config
  (general-nvmap "C-s" 'avy-goto-char-timer)
  (+general-global-goto "g" 'avy-goto-line)
  :custom
  (avy-timeout-seconds 0.35)
  (avy-single-candidate-jump nil))

(use-package expreg
  :general-config
  (general-nvmap "RET" 'expreg-expand)
  (general-nvmap "S-<return>" 'expreg-contract))

(use-package doom-modeline
  :config
  (setq-default mode-line-buffer-identification "%b")
  (setq doom-modeline-mode-alist nil)
  (doom-modeline-def-modeline 'my-modeline
			      '(matches buffer-info remote-host buffer-position)
			      ;; TODO: checker segment was renamed to check in #62890ef
			      '(misc-info time irc debug input-method major-mode process checker))
  (add-hook 'doom-modeline-mode-hook
	    (lambda nil
	      (doom-modeline-set-modeline 'my-modeline 'default)))
  (doom-modeline-mode 1)
  (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-highlight-modified-buffer-name nil)
  (doom-modeline-irc-buffers t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-time-icon nil))

(use-package frame
  :custom (window-divider-default-right-width 7)
  :config (window-divider-mode 1))

(use-package time
  :after (exwm)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "[%d/%b %H:%M]")
  :config (display-time-mode))

(use-package desktop-environment
  :after (exwm)
  :config
  (setq desktop-environment-update-exwm-global-keys :prefix)
  (define-key desktop-environment-mode-map (kbd "s-l") nil)
  (desktop-environment-mode))

;;; completion

(use-package minibuffer
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (tab-always-indent 'complete)
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package savehist
  :init (savehist-mode)
  :custom
  (savehist-file (var "savehist.el"))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 300)
  (savehist-ignored-variables '(file-name-history))
  (savehist-additional-variables '(kill-ring register-alist
					     mark-ring global-mark-ring
					     search-ring regexp-search-ring))
  (history-length 1000)
  (history-delete-duplicates t)
  :config
  (defun my/savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
	  (mapcar #'substring-no-properties
		  (cl-remove-if-not #'stringp kill-ring))
	  register-alist
	  (cl-loop for (reg . item) in register-alist
		   if (stringp item)
		   collect (cons reg (substring-no-properties item))
		   else collect (cons reg item))))

  (defun my/savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwriteable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
		(cl-remove-if-not #'savehist-printable register-alist)))

  (add-hook 'savehist-save-hook 'my/savehist-unpropertize-variables-h)
  (add-hook 'savehist-save-hook 'my/savehist-remove-unprintable-registers-h))

(use-package orderless
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(defun my/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale ~/guix-dotfiles/home/files/emacs/exwm/guix.png"))

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

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

(defun my/polybar-exwm-workspace ()
  "Switch case to select the appropriate indicator."
  (pcase exwm-workspace-current-index
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")))

(defun my/send-polybar-hook (module-name hook-index)
  "Generic IPC hook function for communicating with Polybar."
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun my/send-polybar-exwm-workspace ()
  (my/send-polybar-hook "exwm-workspace" 1))

(defun my/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  ;; (exwm-workspace-switch-create 1)
  ;; Start the Polybar panel
  ;; (my/start-panel)
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
  (add-hook 'exwm-workspace-switch-hook #'my/send-polybar-exwm-workspace)
  
  ;; Set the wallpaper after changing the resolution
  ;; (my/set-wallpaper)
  
  ;; Set starting workspace to 1
  ;; (setq exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))
  
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

;; Set the screen resolution (update this to be the correct resolution for your screen)
(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")  

(use-package vertico
  :init (vertico-mode))

(use-package app-launcher)
