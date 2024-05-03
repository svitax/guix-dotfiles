(define-module (fennec packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system font))

(define-public font-nerd-fonts-iosevka-term
  (package
   (name "font-nerd-fonts-iosevka-term")
   (version "3.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
	   version
	   "/IosevkaTerm.zip"))
     (sha256
      (base32 "1010anwp2a4w2ahmcvgkhfg1fpr82fm6fldbrridfp119z27yb2d"))))
   (build-system font-build-system)
   (home-page "https://github.com/ryanoasis/nerd-fonts")
   (synopsis "The Iosevka Term font, patched with nerd-fonts")
   (description "Nerd Fonts is a project that patches developer targeted fonts with a high number of glyphs (icons). Specifically to add a high number of extra glyphs from popular 'iconic fonts' such as Font Awesome, Devicons, Octicons, and others.")
   (license license:expat)))

(define-public fennec-font-cozette
  (package
    (name "fennec-font-cozette")
    (version "1.23.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/slavfox/Cozette")
                     (commit (string-append "v." version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x43c3w1apxw4yva2hz14lsbh4mzsg3pfjcm25iawmsgkfn4h76x"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-depend-on-git
           (lambda _
             (substitute* "build.py"
               ;; Merely importing this module requires a git repository.
               ;; We don't use get_changelog, so just disable the import.
               (("from cozette_builder\\.changeloggen import get_changelog, get_last_ver")
                ""))))
         (add-before 'install 'build
           (lambda _
             (invoke "python3" "build.py" "fonts"))))))
    (native-inputs
     (list fontforge
           python
           python-crayons
           python-fonttools
           python-numpy
           python-pillow))
    (home-page "https://github.com/slavfox/Cozette")
    (synopsis "Bitmap programming font")
    (description "Cozette is a 6x13px (bounding box) bitmap font based on Dina
and heavily inspired by Creep.")
    (license license:expat)))
