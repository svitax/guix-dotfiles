;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
	     (gnu home services)
	     (gnu home services ssh)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list "git"
					   "make"
					   "feh"
					   "xrandr"
					   "alsa-utils"
					   "font-iosevka-comfy"
					   "font-nerd-fonts-iosevka-term"
					   "polybar"
					   "surf"
					   ;; "qutebrowser"
					   "icecat"
					   "st"
					   ;; Emacs packages
					   "emacs-general"
					   "emacs-evil"
					   "emacs-evil-collection"
					   "emacs-anzu"
					   "emacs-evil-anzu"
					   "emacs-evil-surround"
					   "emacs-modus-themes"
					   "emacs-ef-themes"
					   "emacs-rainbow-mode"
					   "emacs-fontaine"
					   "emacs-hl-todo"
					   "emacs-consult-todo"
					   "emacs-crux"
					   "emacs-visual-regexp"
					   "emacs-vundo"
					   "emacs-undo-fu-session"
					   "emacs-avy"
					   "emacs-expreg"
					   "emacs-doom-modeline"
					   "emacs-orderless"
					   "emacs-corfu"
					   "emacs-cape"
					   "emacs-consult"
					   "emacs-consult-dir"
					   "emacs-embark"
					   "emacs-marginalia"
					   "emacs-vertico"
					   "emacs-diredfl"
					   "emacs-dired-single"
					   "emacs-dired-imenu"
					   "emacs-dired-sort-by"
					   "emacs-desktop-environment"
					   "emacs-app-launcher"
					   "emacs-vterm"
					   "emacs-geiser"
					   "emacs-geiser-guile"
					   "guile")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
                              ("ls" . "ls -p --color=auto")))
                   (bashrc (list (local-file
                                  "/home/svitax/guix-dotfiles/home/files/bash/.bashrc"
                                  "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/svitax/guix-dotfiles/home/files/bash/.bash_profile"
                                        "bash_profile")))))
	 (simple-service 'some-useful-env-vars-service
			 home-environment-variables-service-type
			 `(("EDITOR" . "emacsclient -n -r")
			   ("ALTERNATE_EDITOR" . "emacsclient -n -r")))
	 (service home-openssh-service-type)
	 (service home-ssh-agent-service-type)
	 (simple-service 'dot-configs-service
			 home-files-service-type
			 `((".config/emacs/init.el"
			    ,(local-file "files/emacs/init.el"))
			   (".config/emacs/early-init.el"
			    ,(local-file "files/emacs/early-init.el"))
			   ;; TODO: create .config/cache/emacs/var and etc/ dirs to avoid errors
			   (".config/polybar"
			    ,(local-file "files/polybar" #:recursive? #t))
			   ;; (".config/polybar/config.ini"
			   ;;  ,(local-file "files/polybar/config.ini"))
			   )))))
