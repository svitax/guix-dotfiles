;;; init.el -*- lexical-binding: t; -*-

;;;; package

(add-to-list 'load-path "~/.guix-home/profile/share/emacs/site-lisp")
(guix-emacs-autoload-packages)

;; "plugins/" contains downloaded packages or plugins I've written
(add-to-list 'load-path (concat user-emacs-directory "plugins"))

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-enable-imenu-support t
	use-package-minimum-reported-time 0.01)
  (require 'use-package))

(use-package use-package-evil-bind)

;;;; evil

;; Use a custom keymap for leader and local-leader bindings
(define-prefix-command 'my/leader-map)
(define-prefix-command 'my/local-leader-map)

;; TODO: add evil help-mode config to a help use-package declaration.
(use-package evil
  :evil-bind ((:map (evil-insert-state-map)
		    ;; Use this instead of the default `evil-complete-next', as
		    ;; `completion-at-point' integrates with Consult etc. by
		    ;; default.
		    ;; TODO: C-n and C-p in corfu-mode don't complete
		    ;; ("C-n" . completion-at-point)
		    ;; Emacs movement
		    ("C-a" . my/move-indentation-or-beginning-of-line)
		    ("C-e" . move-end-of-line)
		    ("C-y" . yank)
		    ("C-t" . transpose-chars))
	      (:map (evil-visual-state-map)
		    ("SPC" . my/leader-map)
		    ("," . my/local-leader-map)
		    ("H" . my/move-indentation-or-beginning-of-line)
		    ("L" . move-end-of-line))
	      (:map (evil-normal-state-map)
		    ("SPC" . my/leader-map)
		    ("," . my/local-leader-map)
		    ("H" . my/move-indentation-or-beginning-of-line)
		    ("L" . move-end-of-line)
		    ("(" . evil-previous-open-paren)
		    (")" . evil-next-close-paren)
		    ("j" . evil-next-visual-line)
		    ("k" . evil-previous-visual-line)
		    ("M-j" . my/move-line-down)
		    ("M-k" . my/move-line-up)
		    ("M-h" . evil-shift-left-line)
		    ("M-l" . evil-shift-right-line)
		    ("U" . evil-redo))
	      ;; The *Warnings* buffer loads in normal mode, and I want to be
	      ;; able to quit it easily
	      (:map (special-mode-map . normal)
		    ("q" . quit-window))
	      ;; Help bindings
	      (:map (help-mode-map . normal)
		    ("q" . quit-window)
		    ("C-i" . help-go-forward)
		    ("C-o" . help-go-back)
		    ("<RET>" . help-follow-symbol)))
  :custom
  (evil-want-keybinding nil)
  (evil-symbol-word-search t)
  (evil-echo-state nil "Don't put insert/visual etc in minibuffer")
  (evil-goto-definition-functions
   '(evil-goto-definition-xref
     evil-goto-definition-imenu
     evil-goto-definition-semantic
     evil-goto-definition-search)
   "Prefer xref for jumping to definition, as this means we get the Eglot
jump-to feature when enabled. By default this prefers imenu.")
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t "Isn't working for some reason")
  (evil-respect-visual-line-mode t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-want-fine-undo t)
  :preface
  (defun my/move-indentation-or-beginning-of-line ()
    (interactive)
    (if (= (point) (progn (beginning-of-line-text) (point)))
	(beginning-of-line)))

  (defun my/move-line-up ()
    "Move the current line up one row."
    (interactive)
    (let ((col (current-column))
          ;; For org-agenda I like that you can temporarily change order of items, so ignore readonly
          (inhibit-read-only (eq major-mode 'org-agenda-mode)))
      (transpose-lines 1)
      (forward-line -2)
      (evil-goto-column col)))

  (defun my/move-line-down ()
    "Move the current line down one row."
    (interactive)
    (let ((col (current-column))
          ;; For org-agenda I like that you can temporarily change order of items, so ignore readonly
          (inhibit-read-only (eq major-mode 'org-agenda-mode)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (evil-goto-column col)))
  :init (evil-mode)
  :config
  ;; I keep accidentally quitting with :q. Just deleting the window is enough.
  (evil-ex-define-cmd "q[uit]" 'evil-window-delete)
  ;; Use vi keys to navigate `help-mode'
  (evil-set-initial-state 'help-mode 'normal))

(use-package evil-collection
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  :init (evil-collection-init))

(use-package evil-surround
  :evil-bind ((:map (global-map . visual)
		    ("s" . evil-surround-region)))
  :init (global-evil-surround-mode))

;;;; files

;; TODO: rename my-no-littering to no-litter
(use-package my-no-littering)

(use-package files
  :evil-bind ((:map (my/leader-map)
		    ("a" . find-file))
	      (:map (my/local-leader-map)
		    ("s" . save-buffer)
		    ("S" . save-some-buffers)))
  :custom
  (make-backup-files nil)
  (backup-directory-alist (file-name-concat user-cache-directory "backup"))
  (backup-directory-alist
   `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
     ("\\`/tmp/" . nil)
     ("\\`/dev/shm/" . nil)
     ("." . ,(var "backup/"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (auto-save-default nil)
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  (auto-save-list-file-prefix (var "auto-save/sessions/"))
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
     ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
     ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
     (".*" ,(var "auto-save/") t)))
  (find-file-suppress-same-file-warnings t)
  ;; TODO: find a place to put these variables
  (echo-keystrokes 0.01)
  (server-client-instructions nil)
  (set-mark-command-repeat-pop t)
  (cycle-spacing-actions '(delete-all-space just-one-space restore))
  (words-include-escapes t)
  (view-read-only t) ;; TODO: view-mode
  (vc-follow-symlinks t)
  (word-wrap t)
  (kill-do-not-save-duplicates t)
  (y-or-n-p-use-read-key t)
  (use-short-answers t))

;; TODO: vertico truncate so recentf file names in /nix/store/ don't get too big
;; TODO: maybe don't include /guix/store/ items from recentf in recentf-extras
(use-package recentf
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  (recentf-save-file (var "recentf-save.el"))
  :config
  ;; Keep track of recently opened files. Also feeds into the list of recent
  ;; directories used by consult-dir.
  (defun doom--recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
	(abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

  ;; Exclude anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))
  ;; Exclude anything in nix store
  (add-to-list 'recentf-exclude (concat "^" (regexp-quote "/nix/store")))

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more reaadable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)
  ;; Text properties inflate the size of recentf's files, and there is no
  ;; purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (defun doom--recentf-touch-buffer-h ()
    "Bump file in recent file list when it is switched or written to."
    (when buffer-file-name
      (recentf-add-file buffer-file-name))
    ;; Return nil for `write-file-functions'
    nil)

  (add-hook 'on-switch-window-hook 'doom--recentf-touch-buffer-h)
  (add-hook 'write-file-functions 'doom--recentf-touch-buffer-h)

  (defun doom--recentf-add-dired-directory-h ()
    "Add Dired directories to recentf file list."
    (recentf-add-file default-directory))

  (add-hook 'dired-mode-hook 'doom--recentf-add-dired-directory-h)

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recent-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package saveplace
  :custom (save-place-file (var "save-place.el"))
  :init (save-place-mode))

(use-package cus-edit
  :custom (custom-file (file-name-concat user-cache-directory "etc/custom.el")))

(use-package autorevert
  :init (global-auto-revert-mode)
  :custom (global-auto-revert-non-file-buffers t))

;; TODO: rename-file-and-buffer https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L217-L231
;; TODO: move-buffer-file https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L234-L248


;;;; exwm

(use-package exwm
  :init
  (evil-set-initial-state 'exwm-mode 'emacs)

  ;; Hooks for class and title, adapted from docs.
  ;;
  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;;  all windows are probably the same.  Using window titles for them makes
  ;;  more sense.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  (defun my/exwm--format-window-title-icecat (title &optional length)
    "Removes noise from and trims Icecat window titles.
Assumes the Add URL to Window Title extension is enabled and configured to use
@ (at symbol) as separator."
    (let* ((length (or length 65))
	   (title (concat "I# " (replace-regexp-in-string " [--] GNU Icecat" "" title)))
	   (title-and-hostname (split-string title "@" nil " "))
	   (hostname (substring (car (last title-and-hostname)) 0 -1))
	   (page-title (string-join (reverse (nthcdr 1 (reverse title-and-hostname))) " "))
	   (short-title (reverse (string-truncate-left (reverse page-title) length))))
      (if (length> title-and-hostname 1)
	  (concat short-title " @ " hostname)
	(reverse (string-truncate-left (reverse title) length)))))
  (defun my/exwm--format-window-title-* (title)
    "Removes annoying notifications and FPS counters."
    (dolist (regexp '("([[:digit:]]+)" "FPS :[[:digit:]]+"))
      (setq title (replace-regexp-in-string regexp "" title)))
    (string-trim title))
  (defun my/exwm-buffer-name ()
    "Guesses (and formats) the buffer name using the class of the X client."
    (let ((title (my/exwm--format-window-title-* exwm-title))
	  (formatter (intern
		      (format "my/exwm--format-window-title-%s"
			      (downcase exwm-class-name)))))
      (if (fboundp formatter)
	  (funcall formatter title)
	title)))
  (defun my/exwm-update-title ()
    (exwm-workspace-rename-buffer (my/exwm-buffer-name)))
  (add-hook 'exwm-update-title-hook #'my/exwm-update-title)

  (defun my/exwm-configure-window-by-class ()
    (setq-local default-directory (expand-file-name "~/"))
    (pcase exwm-class-name
      ("Icecat"
       (exwm-input-set-simulation-keys
	(append
	 exwm-input-simulation-keys
	 '(([?\C-s] . [?\C-f])
	   ([?\C-g] . [escape])
	   ([?\C-m] . [f6]) ; focus/unfocus address bar
	   ([?\C-i] . [f3]) ; next link in quick find
	   ([?\C-o] . [S-f3]) ; previous link in quick find
	   ([?\C-t] . nil))))) ; prevent accidental tab creation
      ("mpv"
       (exwm-floating-toggle-floating)
       (exwm-layout-toggle-mode-line))))
  (add-hook 'exwm-manage-finish-hook #'my/exwm-configure-window-by-class)

  (defun my/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))
  (defun my/exwm-init-hook ()
    ;; Make workspace 1 be the one where we land at startup
    ;; (exwm-workspace-switch-create 1)
    ;; Start the Polybar panel
    ;; (my/start-panel)
    ;; Launch apps that will run in the background
    ;; (my/run-in-background "dunst")
    )
  (add-hook 'exwm-init-hook #'my/exwm-init-hook)

  (defun my/exwm-consult-yank-pop ()
    "Same as `consult-yank-pop' and paste into exwm buffer.
Stolen from https://github.com/DamienCassou/gpastel#for-exwmcounsel-users
and adapted to use simulations keys to have a common yank keystroke."
    (interactive)
    (let ((inhibit-read-only t)
          (yank-pop-change-selection t))
      (call-interactively #'consult-yank-pop))
    (when (derived-mode-p 'exwm-mode)
      ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (let ((keys (gethash [?\C-y]
                           exwm-input--simulation-keys)))
        (dolist (key keys)
          (exwm-input--fake-key key)))))
  :custom
  (xcb:connection-timeout 10)
  (exwm-workspace-number 1 "Only use a single EXWM workspace")
  (exwm-input-global-keys
   `(([?\M-y] . my/exwm-consult-yank-pop)
     ([?\s-d] . app-launcher-run-app)
     ([?\s-D] . my/consult-shell-command)
     ;; Bind "s-r" to exit char-mode and fullscreen mode.
     ([?\s-r] . exwm-reset)
     ;; Make leader and local-leader easily accessible
     (,(kbd "C-<SPC>") . my/leader-map)
     (,(kbd "C-,") . my/local-leader-map)
     ;; Move between windows
     ([?\s-h] . evil-window-left)
     ([?\s-j] . evil-window-down)
     ([?\s-k] . evil-window-up)
     ([?\s-l] . evil-window-right)
     ;; Swap windows
     ([?\s-H] . windmove-swap-states-left)
     ([?\s-J] . windmove-swap-states-down)
     ([?\s-K] . windmove-swap-states-up)
     ([?\s-L] . windmove-swap-states-right)
     ;; Split windows
     ([?\s-v] . evil-window-vsplit)
     ([?\s-s] . evil-window-split)
     ;; Close windows
     ([?\s-c] . evil-window-delete)
     ;; Toggle fullscreen
     ;; TODO: nbarrientos my/toggle-single-window?
     ([?\s-f] . exwm-layout-toggle-fullscreen)
     ;; TODO: nbarrientos my/term-toggle-line-and-char-mode?
     ;; ([?\s-C] . my/term-toggle-line-and-char-mode)
     ;; Switch workspace
     ([?\s-w] . exwm-workspace-switch)
     ;; TODO: nbarrientos my/exwm-toggle-or-set-buffer-protection
     ;; ([?\s-p] . my/exwm-toggle-or-set-buffer-protection)
     ;; TODO: nbarrientos my/remote-or-local-term (use vterm or eat)
     ;; ([?\s-t] . my/remote-or-local-term)
     (,(kbd "s-<return>") . eshell)
     ([?\s-=] . balance-windows)
     ([?\s-+] . (lambda ()
		  (interactive)
		  (exwm-layout-enlarge-window-horizontally 100)))
     ([?\s--] . (lambda ()
		  (interactive)
		  (exwm-layout-shrink-window-horizontally 100)))
     ;; TODO: nbarrientos my/consult-buffer-ansi-term (use vterm or eat)
     ;; TODO: nbarrientos my/consult-buffer-detached-command
     ;; TODO: nbarrientos my/consult-buffer-firefox
     ,@(mapcar (lambda (i)
		 `(,(kbd (format "s-%d" i)) . (lambda ()
						(interactive)
						(exwm-workspace-switch-create ,i))))
	       (number-sequence 0 9)))
   "EXWM bindings that are always available.")
  (exwm-input-simulation-keys
   `(([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end]))
   "Rebindings that get sent through to X programs, e.g. C-n/C-p can be down/up")
  :evil-bind ((:map (exwm-mode-map)
		    ("M-o" . other-window)
		    ("C-q" . exwm-input-send-next-key)))
		    ;; TODO: window-swap-states in exwm-mode-map
		    ;; TODO: rotate-frame-clockwise in exwm-mode-map
  :config
  (add-to-list 'exwm-input-prefix-keys ?\C-c)
  ;; Update panel indicator when workspace changes
  ;; (add-hook 'exwm-workspace-switch-hook #'my/send-polybar-exwm-workspace)
  
  ;; Set the wallpaper after changing the resolution
  ;; (my/set-wallpaper)
  
  ;; Set starting workspace to 1
  ;; (setq exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))
  
  (exwm-enable))

;; Set the screen resolution (update this to be the correct resolution for your screen)
(use-package exwm-randr
  :after (exwm)
  :config
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal"))

(use-package app-launcher)

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

;;;; guix
;; guix

;;;; theme

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

;; pulsar?
;; spacious-padding

(use-package rainbow-mode
  :hook ((css-mode help-mode html-mode) . rainbow-mode)
  :evil-bind ((:map (my/leader-map)
                    ("xc" . rainbow-mode)))
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil))

(use-package fontaine
  :evil-bind ((:map (my/leader-map)
		    ("xf" . fontaine-set-preset)))
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
  :evil-bind ((:map (my/leader-map)
		    ("gt" . consult-todo))))

;;;; editing

(use-package crux
  ;; BUG: evil-visual-line is including newline? and causes `crux-duplicate-current-line-or-region' to include next line 
  :evil-bind ((:map (global-map . normal)
		    ("R" . crux-duplicate-current-line-or-region))))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package visual-regexp
  :evil-bind ((:map (my/leader-map)
		    ("r" . vr/query-replace))))

(use-package vundo
  :evil-bind ((:map (my/leader-map)
		    ("u" . vundo)))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  (vundo-window-max-height 8))

(use-package undo-fu-session
  :custom (undo-fu-session-directory (var "undo-fu-session/"))
  :init (undo-fu-session-global-mode))

(use-package newcomment
  :evil-bind ((:map (global-map)
		    ("M-;" . comment-line)
		    ("M-RET" . comment-indent-new-line)))
  :hook ((prog-mode . (lambda ()
			(set (make-local-variable
			      'comment-auto-fill-only-comments)
			     t)))))

;;;; navigation

(use-package avy
  :evil-bind ((:map (global-map . (normal visual))
		    ("C-s" . avy-goto-char-timer))
	      (:map (my/leader-map)
		    ("gg" . avy-goto-line)))
  :custom
  (avy-timeout-seconds 0.35)
  (avy-single-candidate-jump nil))

(use-package expreg
  :evil-bind ((:map (global-map . (normal visual))
		    ("RET" . expreg-expand)
		    ("S-<return>" . expreg-contract))))

(use-package evil-matchit
  :init (global-evil-matchit-mode))

;; dumb-jump?
;; harpoon https://github.com/kofm/harpoon.el
;; ace-link https://github.com/abo-abo/ace-link
;; buffer-local-xref https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L2454
;; smart-tab-over?
;; treesit-jump? https://github.com/dmille56/treesit-jump

;;;; modeline

;; mood-line?

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

;; keycast

;;;; completion

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
  :evil-bind ((:map (minibuffer-local-map . normal)
		    ("gk" . previous-history-element)
		    ("gj" . next-history-element)))
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

(use-package vertico
  :evil-bind ((:map (vertico-map . (normal insert emacs))
		    ("C-u" . vertico-scroll-down)
		    ("C-d" . vertico-scroll-up)
		    ("C-l" . vertico-exit-input)
		    ("C-j" . vertico-next-group)
		    ("C-k" . vertico-previous-group)
		    ("M-q" . vertico-quick-jump)))
  :custom
  (vertico-count 10 "Items displayed, defaults to 10")
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  :init (vertico-mode)
  :config (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :evil-bind ((:map (vertico-map . (normal insert emacs))
		    ("RET" . vertico-directory-enter)
		    ("DEL" . vertico-directory-delete-char)
		    ("C-w" . vertico-directory-delete-word)
		    ("M-DEL" . vertico-directory-delete-word))))

(use-package vertico-multiform
  :after vertico
  :hook (vertico-mode . vertico-multiform-mode)
  :custom (vertico-multiform-commands
	   '((Info-menu (vertico-sort-function . nil)))))

(use-package corfu
  :hook
  ((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
  ((eshell-mode shell-mode comint-mode) . my/corfu-shell-settings)
  (minibuffer-setup . my/corfu-enable-in-minibuffer)
  :evil-bind ((:map (corfu-map)
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-insert)
              ("RET" . nil)
              ("M-h" . nil)
              ("C-h" . corfu-info-documentation)
              ("M-m" . my/corfu-move-to-minibuffer)
              ("M-." . corfu-info-location)))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.15)
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 5)
  (corfu-sort-override-function 'my/corfu-combined-sort)
  :preface
  (defun my/corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
		corfu-auto nil)
    (setq-local corfu-map (copy-keymap corfu-map)
		completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))

  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popupinfo
		  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun my/corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
	     completion-cycle-threshold completion-cycling)
	 (consult-completion-in-region beg end table pred)))))

  (defun my/corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
	   (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
	     (if display-sort-func
		 (funcall display-sort-func candidates)
	       candidates))))
      (if corfu-sort-function
	  (funcall corfu-sort-function candidates)
	candidates)))
  :config
  (add-to-list 'corfu-continue-commands #'my/corfu-move-to-minibuffer))

(use-package corfu-popupinfo
  :after corfu
  :config (corfu-popupinfo-mode 1)
  :evil-bind ((:map (corfu-map)
              ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))
  :custom (corfu-popupinfo-delay '(1.0 . 0.05)))

(use-package corfu-echo
  :after corfu
  :config (corfu-echo-mode)
  :custom (corfu-echo-delay '(0.15 . 0.05)))

(use-package corfu-history
  :after corfu
  :init (corfu-history-mode 1)
  :config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-quick
  :after corfu
  :evil-bind ((:map (corfu-map)
		    ("M-q" . corfu-quick-insert)))
  :custom (corfu-quick1 "asdfghjkl;"))

(use-package cape
  :evil-bind ((:map (global-map . normal)
		    ("M-/" . cape-dabbrev)))
  :custom (cape-dabbrev-check-other-buffers nil))

;; TODO: cape extras

(use-package consult
  :hook ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :evil-bind ((:map (my/leader-map)
	      ("b" . consult-buffer)
	      ("q" . consult-kmacro)
	      ("y" . consult-yank-pop)
	      ;; search
	      ("sd" . consult-find)
	      ("sD" . consult-locate)
	      ("se" . consult-isearch-history)
	      ("sg" . consult-grep)
	      ("sG" . consult-git-grep)
	      ("si" . consult-info)
	      ("sk" . consult-keep-lines)
	      ("sm" . consult-map)
	      ("sr" . consult-recent-file)
	      ("su" . consult-focus-lines)
	      ("sx" . consult-mode-command)
	      ("s/" . consult-ripgrep)
	      ("s;" . consult-complex-command)
	      ;; goto
	      ("ge" . consult-compile-error)
	      ("gf" . consult-flymake)
	      ("gG" . consult-goto-line)
	      ("gi" . consult-imenu)
	      ("gI" . consult-imenu-multi)
	      ("gk" . consult-bookmark)
	      ("gl" . consult-line)
	      ("gL" . consult-line-multi)
	      ("gm" . consult-mark)
	      ("gM" . consult-global-mark)
	      ("go" . consult-outline))
	      (:map (project-prefix-map)
		    ("b" . consult-project-buffer))
	      (:map (isearch-mode-map)
		    ("C-r" . consult-isearch-history))
	      (:map (minibuffer-local-map . (normal insert emacs))
		    ("C-r" . consult-history))
	      (:map (consult-narrow-map)
		    ("?" . consult-narrow-help)))
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-async-input-throttle 0.1)
  (consult-async-refresh-delay 0.1)
  (consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     consult--source-project-buffer))
  :init
  (defun my/consult-shell-command ()
    (interactive)
    (let* ((candidates (split-string
			(shell-command-to-string "compgen -c")
			"\n"
			t))
	   (command (consult--read
		     candidates
		     :prompt "Shell command: ")))
      (start-process-shell-command command nil command)))

  (defun my/consult-buffer-by-prefix (prefix caller show-preview)
    "Use consult to select a buffer prefixed by PREFIX#.

Show buffer previews if SHOW-PREVIEW is not nil."
    (let* ((consult--customize-alist
            (if show-preview
                (remove (list caller :preview-key nil) consult--customize-alist)
              consult--customize-alist))
           (my/consult--source-buffer-prefixed
            `(:name ,(format "Buffers (%s)" prefix)
              :category buffer
              :face consult-buffer
              :history buffer-name-history
              :state ,#'consult--buffer-state
              :default t
              :items
              ,(lambda ()
                 (consult--buffer-query
                  :sort 'visibility
                  :include (concat "^" prefix "#")
                  :as #'buffer-name))))
           (consult-buffer-sources (list my/consult--source-buffer-prefixed)))
      (consult-buffer)))
  (defun my/consult-buffer-icecat (arg)
    "Use consult to select an Icecat buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "I" this-command arg))
  (defun my/consult-buffer-ansi-term (arg)
    "Use consult to select an ansi-term buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "U" this-command arg))
  (defun my/consult-buffer-detached-command (arg)
    "Use consult to select a compilation buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "D" this-command arg))
  :config
  ;; Don't preview Icecat buffers
  (consult-customize my/consult-buffer-icecat :preview-key nil)
  ;; Hide recent files list (still available with "f" prefix)
  (consult-customize consult--source-recent-file :hidden t)
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

;; TODO: can I save consult-dir history with savehist?
(use-package consult-dir
  :evil-bind ((:map (my/leader-map)
		    ("j" . consult-dir))
	      ;; TODO: consult-dir in minibuffer/vertico
	      (:map (minibuffer-local-filename-completion-map)
		    ("C-M-j" . consult-dir)
		    ("C-M-z" . consult-dir-jump-file))
	      ;; (:map (embark-become-file+buffer-map)
	      ;; 	    ("d" . consult-dir))
	      )
  :custom (consult-dir-default-command #'project-find-file))

;; FIXME: embark alist?
(use-package embark
  :evil-bind ((:map (global-map . (normal insert emacs))
		    ("C-." . embark-act)
		    ("C-:" . embark-act-all)
		    ("M-." . embark-dwim))
	      (:map (help-map)
		    ("b" . embark-bindings)
		    ("B" . embark-bindings-at-point)
		    ("M" . embark-bindings-in-keymap)
		    ("C-h" . embark-prefix-help-command))
	      (:map (vertico-map)
		    ("C-c C-o" . embark-collect)
		    ("C-M-l" . embark-export))
	      (:map (minibuffer-local-map)
		    ("C-<tab>" . my/embark-select))
	      (:map (embark-file-map)
		    ("s" . sudo-edit-find-file)))
  :custom
  (embark-quit-after-action nil)
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command "Replace the key help with a completing-read interface")
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-confirm-act-all nil)
  :preface
  (defun my/embark-select ()
    (interactive)
    (prog1 (embark-select)
      (if (minibufferp)
          (when (bound-and-true-p vertico-mode)
            (vertico-next))
	(call-interactively #'next-line)))))

(use-package embark-consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package marginalia
  :evil-bind ((:map (minibuffer-local-map)
		    ("M-a" . marginalia-cycle)))
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode)
  :config
  (add-to-list 'marginalia--buffer-file :filter-return
	       (lambda (buffer-file)
		 (string-trim-left buffer-file "(compilation\\(<.+>\\)? run) "))))

;;;; search

;; isearch
(use-package isearch
  :evil-bind ((:map (isearch-mode-map)
		    ("M-." . isearch-forward-thing-at-point)))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil))

(use-package anzu
  :custom (anzu-search-threshold 10000)
  :init (global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

;; grep
;; wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; occur
;; isearch-extras
;; imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
;; cc-isearch-menu

;;;; dired

(use-package dired
  :evil-bind ((:map (my/leader-map)
		    ("e" . dired-jump))
	      (:map (dired-mode-map . normal)
		    ("a" . find-file)
		    ("l" . dired-find-file)
		    ("h" . dired-up-directory)
		    ("/" . my/dired-limit-regexp)
		    ("~" . my/dired-home-directory)))
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-NGalhv --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  ;; (dired-create-destination-dirs 'ask)
  ;; (dired-listing-switches "-AGhlvX")
  ;; (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (delete-by-moving-to-trash t)
  :preface
  (defun my/dired-home-directory ()
    (interactive)
    (dired-single-buffer (expand-file-name "~/")))

  (defvar my/dired--limit-hist '()
    "Minibuffer history for `my/dired-limit-regexp'.")
  (defun my/dired-limit-regexp (regexp omit)
    "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
    (interactive
     (list
      (read-regexp
       (concat "Files "
	       (when current-prefix-arg
		 (propertize "NOT " 'face 'warning))
	       "matching PATTERN: ")
       nil 'my/dired--limit-hist)
      current-prefix-arg))
    (dired-mark-files-regexp regexp)
    (unless omit (dired-toggle-marks))
    (dired-do-kill-lines)
    (add-to-history 'my/dired--limit-hist regexp)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-single
  ;; TODO: use :evil-bind for dired-single remaps
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-up-directory] . dired-single-up-directory))

(use-package dired-imenu)

(use-package dired-sort-by
  :evil-bind ((:map (dired-mode-map . normal)
		    ("s" . dired-sort-by))))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; dired-ranger
;; `dired-ranger-copy' and `dired-ranger-paste' and `dired-ranger-move'

;; dired-narrow? to replace my/dired-limit-regexp?
;; `dired-narrow-regexp'

;; dired-subtree
;; dired-preview
;; dired-open vs dired-launch
;; https://codeberg.org/thomp/dired-launch

;; dired-atool?
;; dired-rsync?
;; dired-gitignore?
;; sudo-edit?
;; dired-hide-dotfiles
;; dired-filter? what filters would i use?

;;;; ibuffer

(use-package ibuffer
  :evil-bind ((:map (my/leader-map)
		    ("B" . ibuffer-jump))))

(use-package ibuffer-vc
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :init
  (setq ibuffer-formats '((mark modified read-only vc-status-mini " "
                           (name 18 18 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " "
                           (vc-status 16 16 :left)
                           " "
                           vc-relative-file))))

;; ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; ibuffer-git https://github.com/jrockway/ibuffer-git
;; projection-ibuffer?
;; bufler?

;;;; bookmark
;; dogears (vs better-jumper)
;; bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; blist? https://github.com/emacsmirror/blist


;;;; window

(use-package window
  :evil-bind ((:map (global-map . normal)
		    ("M-o" . other-window))
	      (:map (my/leader-map)
		    ("c" . my/delete-window-or-frame)
		    ("k" . my/kill-this-buffer)
		    ("w" . evil-window-map))
	      (:map (evil-window-map)
		    ("C-h" . nil)))
  :custom
  (window-combination-resize t)
  (even-window-sizes 'height-only)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (split-height-threshold 80)
  (split-width-threshold 125)
  (window-min-height 3)
  (window-min-width 30)
  (indicate-buffer-boundaries t)
  (indicate-empty-lines nil)
  (auto-window-vscroll nil)
  :preface
  (defun my/delete-window-or-delete-frame (&optional window)
    "Delete WINDOW using `delete-window'.
If this is the sole window run `delete-frame' instead. WINDOW
must be a valid window and defaults to the selected one. Return
nil."
    (interactive)
    (condition-case nil
	(delete-window window)
      (error (if (and tab-bar-mode
                      (> (length (funcall tab-bar-tabs-function)) 1))
		 (tab-bar-cose-tab)
               (delete-frame)))))

  (defun my/kill-this-buffer (&optional arg)
    (interactive "P")
    (pcase arg
      ('4 (call-interactively #'kill-buffer))
      (_ (kill-buffer (current-buffer))))))

(use-package frame
  :custom (window-divider-default-right-width 7)
  :config (window-divider-mode 1))

;; hl-line

(use-package whitespace
  :config (global-whitespace-mode)
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  (text-mode . (lambda () (setq show-trailing-whitespace t)))
  (conf-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  (whitespace-style '(face trailing tabs empty big-indent))
  (whitespace-global-modes '(not erc-mode magit-mode)))

;; display-line-numbers
(use-package display-line-numbers
  :config (global-display-line-numbers-mode)
  :custom
  (display-line-numbers-major-tick 0)
  (display-line-numbers-minor-tick 0)
  (display-line-numbers-widen t "Display absolute numbers in narrowed buffers"))

;; centered-cursor
;; uniquify

;;;; vc
;; magit
;; diff-hl or gitgutter

;;;; project

;; TODO: move my/project-remove-project to projection-map
(use-package project
  ;; :general-config
  ;; (general-def :keymaps 'projection-map
  ;;   "DEL" 'my/project-remove-project)
  :evil-bind ((:map (my/leader-map)
		    ("f" . project-find-file)
		    ("p" . project-prefix-map))
	      (:map (project-prefix-map)
		    ("g" . consult-ripgrep)
		    ("s" . project-eshell)))
  :custom
  ;; (project-switch-commands #'project-find-file)
  (project-switch-commands '((project-find-file "Find file")
			     (consult-ripgrep "Ripgrep")
			     (project-find-dir "Find directory")
			     (project-eshell "Eshell")
			     (consult-project-buffer "Buffers")
			     (project-kill-buffers "Kill buffers")
			     ;; (magit-project-status "Magit")
			     ))
  (project-list-file (file-name-concat user-cache-directory "var/projects"))
  :config
  (defun my/project-remove-project ()
    "Remove project from `project--list' using completion."
    (interactive)
    (project--ensure-read-project-list)
    (let* ((projects project--list)
           (dir (completing-read "REMOVE project from list: " projects nil t)))
      (setq project--list (delete (assoc dir projects) projects)))))

;; projel
;; projection
;; projection-multi
;; projection-multi-embark
;; envrc
;; inheritenv
;; exec-path-from-shell

;;;; compile
;; compile
;; compile-multi
;; consult-compile-multi
;; compile-multi-embark

;;;; help

(use-package help
  :evil-bind ((:map (my/leader-map)
		    ("h" . help-command))))

(use-package helpful
  ;; TODO: use :evil-bind for helpful remaps
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   :map help-map
   ("C-." . helpful-at-point)
   ("." . helpful-at-point)
   ("C" . describe-command))
  :evil-bind ((:map (embark-symbol-map)
		    ("h" . helpful-symbol)))
  :custom (helpful-max-buffers 1))

;; TODO: helpful embark
;; TODO: use :evil-bind for helpful embark remaps
(use-package helpful
  :after (helpful embark)
  :bind (:map embark-become-help-map
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("C" . helpful-command)))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package info-colors
  :commands info-colors-fontify-node
  :hook (Info-selection . info-colors-fontify-node))

(use-package transient
  :custom
  (transient-history-file (var "transient/history.el"))
  (transient-levels-file (etc "transient/levels.el"))
  (transient-values-file (etc "transient/values.el")))

;; repeat
;; repeat-help

;;;; org

;;;; eglot
;; eglot
;; consult-eglot

;;;; flymake
;; flymake
;; flymake-collection

;;;; apheleia
;; apheleia

;;;; tree-sitter
;; treesit
;; treesit-auto

;;;; langs

;; elisp
(use-package elisp-fontification)

(use-package elisp-indentation)

(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  ;; TODO: replace <localleader> with my/local-leader-map
  :evil-bind ((:map (my/local-leader-map)
		    ("b" . eval-buffer)
		    ("d" . eval-defun)
		    ("e" . eval-last-sexp)))
  :init (add-hook 'eros-inspect-hooks (lambda () (flymake-mode -1))))

(use-package elisp-demos
  :ensure t
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package highlight-quoted
  :ensure t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; rust-mode
;; python
;; markdown-mode

;;;; snippets
;; tempel
;; eglot-tempel
;; tempel-collection

;;;; debug
;; dape
;; edebug

;;;; term

;; vterm or eat
(use-package vterm
  :after (evil)
  :evil-bind ((:map (vterm-mode-map)
		    ("C-<SPC>" . my/leader-map)
		    ("C-g" . vterm--self-insert))
	      (:map (vterm-mode-map . normal)
		    ("gk" . vterm-previous-prompt)
		    ("gj" . vterm-next-prompt)))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm [%s]"))

;; comint
;; terminal-here

;;;; eshell
;; eshell
;; eshell-syntax-highlighting
;; fish-completion?
;; bash-completion?
;; capf-autosuggest?

;;;; tramp
;; tramp

;;;; docker
;; docker
;; docker-tramp
;; dockerfile-mode
;; flymake-hadolint
;; docker-compose-mode

;;;; biblio
;; citar
;; citar-embark
;; citar-denote

;;;; notes
;; denote
;; consult-denote
;; denote-explore

;;;; reading
;; pdf-tools
;; saveplace-pdf-view
;; nov

;;;; icons
(use-package nerd-icons
  :config
  (add-all-to-list 'nerd-icons-extension-icon-alist
		   (list '(exwm-mode
			   nerd-icons-codicon "nf-cod-browser"
			   :face nerd-icons-purple))))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; TODO: hl-line mode doesn't highlight nerd-icons in dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; TODO: hl-line mode doesn't highlight nerd-icons in ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; pass
;; pass

;;;; email
;; notmuch

;;;; rss
;; elfeed
;; phundrak config elfeed

;;;; read-later
;; wombag

;;;; irc
;; erc

;;;; mpv

;;;; music

;;;; files
;; files
;; recentf
;; saveplace
;; cus-edit
;; autorevert

;;;; workspaces

;;;; ai

;;;; extras
(defun add-all-to-list (list-var elements &optional append compare-fn)
  "Add ELEMENTS to the value of LIST-VAR if it isn't there yet.

ELEMENTS is a list of values. For documentation on the variables APPEND and
COMPARE-FN, see `add-to-list'."
  (let (return)
    (dolist (elt elements return)
      (setq return (add-to-list list-var elt append compare-fn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;

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
