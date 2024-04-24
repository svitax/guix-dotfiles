;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg spice)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Los_Angeles")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "guix")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "svitax")
                  (comment "svitax")
                  (group "users")
                  (home-directory "/home/svitax")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "emacs")
                          (specification->package "emacs-exwm")
			  (specification->package "fennec-emacs-exwm")
			  (specification->package "emacs-general")
			  (specification->package "emacs-evil")
                          (specification->package "emacs-desktop-environment")
			  (specification->package "emacs-app-launcher")
			  (specification->package "emacs-fontaine")
			  (specification->package "emacs-modus-themes")
			  (specification->package "emacs-ef-themes")
			  (specification->package "emacs-vterm")
			  (specification->package "emacs-vertico")
			  (specification->package "emacs-geiser")
			  (specification->package "emacs-geiser-guile")
			  (specification->package "guile")
			  )
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list
	    ;; (service spice-vdagent-service-type)
                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "f1849a2a-ae6c-43a6-9c4f-f3ac5b4df74a")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "a83d4f6d-0164-4021-b8fe-10158a68947f"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
