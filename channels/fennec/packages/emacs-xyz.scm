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
                     ~a --daemon -mm --debug-init
                     exec ~a --exit-with-session ~a \"$@\" -c ~%"
						       (search-input-file inputs "/bin/sh")
						       (search-input-file inputs "/bin/xhost")
						       (search-input-file inputs "/bin/emacs")
						       (search-input-file inputs "/bin/dbus-launch")
						       (search-input-file inputs "/bin/emacsclient"))))
					   (chmod exwm-executable #o555)
					   #t))))))))

(define-public emacs-ef-themes
  (package
    (name "emacs-ef-themes")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/protesilaos/ef-themes")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
	 "1s3mjxyi3ssnvbz5r0j1ghnaz2qz02jmhla9nb22nzc5vzjyx5ys"))))
    (build-system emacs-build-system)
    (home-page "https://protesilaos.com/emacs/ef-themes")
    (synopsis "Colorful and legible themes")
    (description
     "The Ef themes are a collection of light and dark themes for GNU Emacs
whose goal is to provide colorful yet legible options for users who want
something with a bit more flair than the Modus themes.")
    (license license:gpl3+)))

(define-public emacs-general
  (let ((commit "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
        (revision "5"))
    (package
      (name "emacs-general")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/noctuid/general.el")
               (commit commit)))
         (sha256
          (base32 "1jillsr80l4wfbcqsxh3zbgbvmbfih2wcz518mgw9p9pwg4xwvy7"))
         (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (native-inputs
       (list emacs-buttercup emacs-evil emacs-which-key emacs-use-package))
      (arguments
       `(#:tests? #t
         #:test-command '("buttercup" "-L" "test/test-general.el")))
      (home-page "https://github.com/noctuid/general.el")
      (synopsis "More convenient key definitions in emacs")
      (description "@code{general.el} provides a more convenient method for
binding keys in emacs (for both evil and non-evil users).  Like
@code{use-package}, which provides a convenient, unified interface for
managing packages, @code{general.el} is intended to provide a convenient,
unified interface for key definitions.  While this package does implement some
completely new functionality (such as the ability to make vim-style
keybindings under non-prefix keys with an optional timeout), its primary
purpose is to build on existing functionality to make key definition more
clear and concise.  @code{general-define-key} is user-extensible and supports
defining multiple keys in multiple keymaps at once, implicitly wrapping key
strings with (@code{kbd ...}), using named prefix key sequences (like the
leader key in vim), and much more.")
      (license license:gpl3+))))

(define-public emacs-expreg
  (let ((commit "9950c07ec90293964baa33603f4a80e764b0a847"))
    (package
     (name "emacs-expreg")
     (version "1.3.1")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/casouri/expreg")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f9lrr6sf67ivvczdai06sj0211rjqycy2hpxy928brg38wwz33z"))))
     (build-system emacs-build-system)
     (home-page "https://github.com/casouri/expreg")
     (synopsis "Simple expand-region clone")
     (description
      "This is just like expand-region, but (1) we generate all regions at once,
and (2) should be easier to debug, and (3) we out-source language-specific expansions to tree-sitter. Bind 'expreg-expand' and 'expreg-contract' and start using it.")
     (license license:gpl3+))))

(define-public emacs-consult-todo
  (package
    (name "emacs-consult-todo")
    (version "20231022.2059")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liuyinz/consult-todo.git")
             (commit "84f3c9876a285f733d75053076a97cc30f7d8eb9")))
       (sha256
        (base32 "0v336l9dary68i910yvpk9c24b9vrc1cx615hiv9dz8zi1khz8rr"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-consult emacs-hl-todo))
    (home-page "https://github.com/liuyinz/consult-todo")
    (synopsis "Search hl-todo keywords in consult")
    (description
     "Provide commands `consult-todo to search, filter, jump to hl-todo keywords.")
    (license license:gpl3+)))

(define-public emacs-dired-single
  (package
    (name "emacs-dired-single")
    (version "20240131.1148")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/dired-single-" version
                           ".tar"))
       (sha256
        (base32 "18hnjdbs0a1ivv1yxic9l518272bb9wswhvpk7z12h2p0vw7nykh"))))
    (build-system emacs-build-system)
    (home-page "https://codeberg.org/amano.kenji/dired-single")
    (synopsis "Reuse the current dired buffer")
    (description
     "This package reuses the current Dired buffer to visit a directory without
creating a new buffer.")
    (license #f)))

(define-public emacs-dired-imenu
  (package
    (name "emacs-dired-imenu")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DamienCassou/dired-imenu.git")
             (commit "4f6169f9056fe5f9b9a97e9e75f27825a15e05b9")))
       (sha256
        (base32 "0djn4ya34l58yf9gssgm4x531qgcp1ad3wizq1nv3qyn2cjfxm7c"))))
    (build-system emacs-build-system)
    (propagated-inputs (list))
    (home-page "https://github.com/DamienCassou/dired-imenu.git")
    (synopsis "Integrate `imenu' in `dired'.")
    (description
     "Emacs mode that integrates `imenu' in `dired' so you can easily jump to
any file in the current buffer.")
    (license #f)))
