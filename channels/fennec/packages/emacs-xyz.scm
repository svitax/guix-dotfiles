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
                     ~a --daemon -f exwm-enable
                     exec ~a --exit-with-session ~a \"$@\" -c -a "" ~%"
						       (search-input-file inputs "/bin/sh")
						       (search-input-file inputs "/bin/xhost")
						       (search-input-file inputs "/bin/emacs")
						       (search-input-file inputs "/bin/dbus-launch")
						       (search-input-file inputs "/bin/emacsclient"))))
					   (chmod exwm-executable #o555)
					   #t))))))))

(define-public emacs-pcache
  (package
   (name "emacs-pcache")
   (version "20220724.1841")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/sigma/pcache.git")
                  (commit "507230d094cc4a5025fe09b62569ad60c71c4226")))
            (sha256
             (base32
	      "1fjdn4g9ww70f3x6vbzi3gqs9dsmqg16isajlqlflzw2716zf2nh"))))
   (build-system emacs-build-system)
   (home-page "unspecified")
   (synopsis "persistent caching for Emacs.")
   (description
    "pcache provides a persistent way of caching data, in a hashtable-like structure.
 It relies on `eieio-persistent in the backend, so that any object that can be
serialized by EIEIO can be stored with pcache.  pcache handles objects called
\"repositories\" (`pcache-repository') and \"entries\" (`pcache-entry').  Each
repository is identified by a unique name, that defines an entry in
`pcache-directory'.  Subdirectories are allowed, by the use of a directory
separator in the repository name.  Example: (let ((repo (pcache-repository
\"plop\"))) (pcache-put repo foo 42) ; store value 42 with key foo (pcache-get
repo foo) ; => 42 ) Keys can be pretty much any Lisp object, and are compared
for equality using `eql Optionally, cache entries can expire: (let ((repo
(pcache-repository \"plop\"))) (pcache-put repo foo 42 1) ; store value 42 with
key foo for 1 second (sleep-for 1) (pcache-get repo foo) ; => nil )")
   (license #f)))

(define-public emacs-logito
  (package
   (name "emacs-logito")
   (version "20201226.534")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/sigma/logito.git")
                  (commit "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124")))
            (sha256
             (base32
	      "0bnkc6smvaq37q08q1wbrxw9mlcfbrax304fxw4fx7pc1587av0d"))))
   (build-system emacs-build-system)
   (home-page "unspecified")
   (synopsis "logging library for Emacs")
   (description "This module provides logging facility for Emacs")
   (license #f)))

(define-public emacs-fontsloth
  (package
   (name "emacs-fontsloth")
   (version "20211118.2018")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jollm/fontsloth.git")
                  (commit "5572a44e14d6c00a628f58cc695c735ef64e0ebd")))
            (sha256
             (base32
              "17q9fqbzzdvl8isj498cjr75bk94n2jp514fsdmlw44s0xnfdk4y"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-f emacs-logito emacs-pcache emacs-stream))
   (home-page "https://github.com/jollm/fontsloth")
   (synopsis "Elisp otf/ttf font loader/renderer")
   (description
    "fontsloth: the slowest font renderer in the world written in pure elisp inspired
by fontdue, the fastest font renderer in the world, written in pure rust *Please
see the website for a detailed README.* To use this module, load and enable it
as follows: (use-package fontsloth) If you also want layout functions (includes
fontsloth): (use-package fontsloth-layout)")
   (license license:gpl3+)))

(define-public emacs-backlight
  (package
   (name "emacs-backlight")
   (version "20210513.129")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mschuldt/backlight.el.git")
                  (commit "b6826a60440d8bf440618e3cdafb40158de920e6")))
            (sha256
             (base32
	      "0nj5l0wwza1j908n9k0896b972b84s401szkgb0acf4fs834vc0w"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/mschuldt/backlight.el")
   (synopsis "backlight brightness adjustment on GNU/Linux")
   (description
    "This package provides a simple utility for setting backlight brightness on some
GNU/Linux systems using sysfs files.  This works like most system provided
backlight brightness controls but allows for increased resolution when the
brightness percentage nears zero.  On some systems a udev rule must be added, in
/etc/udev/rules.d/backlight.rules add: ACTION==\"add\", SUBSYSTEM==\"backlight\",
KERNEL==\"acpi_video0\", GROUP=\"video\", MODE=\"then\" then reload with: sudo udevadm
control --reload-rules && udevadm trigger USAGE M-x backlight Then use < or > to
adjust backlight brightness, C-g when done.  M-x backlight-set-raw prompts for a
value to write directly to the device file.")
   (license #f)))

(define-public emacs-volume
  (package
   (name "emacs-volume")
   (version "20220904.1727")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/dbrock/volume.el.git")
                  (commit "050d3e6d2543a6771a13f95612055864679b6301")))
            (sha256
             (base32
	      "1vyl13swx82njqlfzmaj9c4vbdpdsj4m9f8v32a9kycdhbm9x90z"))))
   (build-system emacs-build-system)
   (home-page "http://www.brockman.se/software/volume-el/")
   (synopsis "tweak your sound card volume from Emacs")
   (description
    "To use this program, put this file in your `load-path', and put the following
autoload in your ~/.emacs: (autoload volume \"volume\" \"Tweak your sound card
volume.\" t) Then type `M-x volume <RET> to run the program.  Of course, use `M-x
customize-group <RET> volume <RET> to customize it.  Tweaking the volume of my
music used to be one of the few things I constantly went outside of Emacs to do.
 I just decided I've had enough of that, and so I wrote this simple mixer
frontend.  It comes with backend glue for aumix and amixer, but the latter is
pretty slow, so I have to recommend the former.  If you can't use either,
writing your own glue should be straightforward.  And if you do, please consider
sending the code to me, so I can integrate it into this file.")
   (license #f)))

(define-public emacs-exlybar
  (package
   (name "emacs-exlybar")
   (version "0.22.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jollm/exlybar.git")
                  (commit "v0.22.4")))
            (sha256
             (base32
	      "0djy7404zllbyl3fmqzqji046imxsjby39g31vs9223n5bf0882s"))))
   (build-system emacs-build-system)
   (propagated-inputs (list
		       emacs-f
		       emacs-s
		       emacs-dash
		       emacs-xelb
		       emacs-fontsloth
		       emacs-log4e
		       emacs-backlight
		       emacs-volume
		       emacs-all-the-icons))
   (arguments
    '(#:include '("^[^/]*\\.el$" "^[^/]*\\.info$" "^doc/.*\\.info$" "^modules/[^/]*\\.el$")))
   (home-page "https://github.com/jollm/exlybar")
   (synopsis "Emacs polybar-like thing")
   (description
    "This module uses xelb to build polybar like modules for displaying status information.

     *Please see the website for a detailed README.*

     To use this module, load and enable it as follows:
     (use-package exlybar
       :config (exlybar))")
   (license license:gpl3+)))
