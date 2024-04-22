(define-module (fennec packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-exwm
  (package
    (name "emacs-exwm")
    (version "0.28")
    (synopsis "Emacs X window manager")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/"
                           "exwm-" version ".tar"))
       (sha256
        (base32 "00h5awqazk807zxvb02a9dp8gd5ifi3y1kcwmr1czk6kdmkjx32l"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-xelb))
    (inputs
     (list xhost dbus))
    ;; The following functions and variables needed by emacs-exwm are
    ;; not included in emacs-minimal:
    ;; scroll-bar-mode, fringe-mode
    ;; x-display-pixel-width, x-display-pixel-height
    (arguments
     `(#:emacs ,emacs
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-xsession
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (bin (string-append out "/bin"))
                    (exwm-executable (string-append bin "/exwm")))
               ;; Add a .desktop file to xsessions
               (mkdir-p xsessions)
               (mkdir-p bin)
               (make-desktop-entry-file
                (string-append xsessions "/exwm.desktop")
                #:name ,name
                #:comment ,synopsis
                #:exec exwm-executable
                #:try-exec exwm-executable)
               ;; Add a shell wrapper to bin
               (with-output-to-file exwm-executable
                 (lambda _
                   (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a \"$@\" --eval '~s' ~%"
                           (search-input-file inputs "/bin/sh")
                           (search-input-file inputs "/bin/xhost")
                           (search-input-file inputs "/bin/dbus-launch")
                           (search-input-file inputs "/bin/emacs")
                           '(cond
                             ((file-exists-p "~/.exwm")
                              (load-file "~/.exwm"))
                             ((not (featurep 'exwm))
                              (require 'exwm)
                              (require 'exwm-config)
                              (exwm-config-default)
                              (message (concat "exwm configuration not found. "
                                               "Falling back to default configuration...")))))))
               (chmod exwm-executable #o555)
               #t))))))
    (home-page "https://github.com/ch11ng/exwm")
    (description
     "EXWM is a full-featured tiling X window manager for Emacs built on top
of XELB.")
    (license license:gpl3+)))

(define-public fennec-emacs-exwm
  (package
   (inherit emacs-exwm)
   (name "fennec-emacs-exwm")
   (propagated-inputs (list emacs-xelb))
   (arguments
    `(#:emacs ,emacs
      #:phases
      (modify-phases %standard-phases
		     (add-after 'build 'install-xsession
				(lambda* (#:key inputs outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(xsessions (string-append out "/share/xsessions"))
						(bin (string-append out "/bin"))
						(exwm-executable (string-append bin "/exwm")))
					   ;; Add a .desktop file to xsessions
					   (mkdir-p xsessions)
					   (mkdir-p bin)
					   (make-desktop-entry-file (string-append xsessions "/emacs.desktop")
								    #:name "Lisp Machine (Emacs)"
								    #:exec exwm-executable
								    #:try-exec exwm-executable)
					   ;; Add a shell wrapper to bin
					   (with-output-to-file exwm-executable
					     (lambda _
					       (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a \"$@\" -mm --debug-init ~%"
						       (search-input-file inputs "/bin/sh")
						       (search-input-file inputs "/bin/xhost")
						       (search-input-file inputs "/bin/dbus-launch")
						       (search-input-file inputs "/bin/emacs"))))
					   (chmod exwm-executable #o555)
					   #t))))))))
