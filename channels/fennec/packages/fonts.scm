(define-module (fennec packages fonts)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 string-fun)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
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

(define* (install #:key outputs #:allow-other-keys)
  "Install the package contents."
  (let* ((out (assoc-ref outputs "out"))
         (source (getcwd))
         (fonts (string-append out "/share/fonts")))
    (for-each (cut install-file <> (string-append fonts "/truetype"))
              (find-files source "\\.(ttf|ttc)$"))
    (for-each (cut install-file <> (string-append fonts "/opentype"))
              (find-files source "\\.(otf|otc)$"))
    (for-each (cut install-file <> (string-append fonts "/web"))
              (find-files source "\\.(woff|woff2)$"))))

(define-public fennec-font-cozette
  (package
    (name "fennec-font-cozette")
    (version "1.23.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		    "https://github.com/slavfox/Cozette/releases/download/v."
		    version
		    "/CozetteFonts-v-"
		    (string-replace-substring version "." "-")
		    ".zip"))
	      (sha256
               (base32
                "0j5pfd28s6zibgm4l3yasf5ysfq24bfxd1g4g1wdkgj7zfmqyhl8"))))
    (build-system font-build-system)
    (outputs '("out"
	       "bdf" "otf" "pcf" "psf"))
    (arguments
     `(#:phases
      (modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((otb (assoc-ref outputs "out"))
                     (bdf (assoc-ref outputs "bdf"))
                     (otf (assoc-ref outputs "otf"))
                     (pcf (assoc-ref outputs "pcf"))
                     (psf (assoc-ref outputs "psf"))
                     (otb-font-dir (string-append (assoc-ref outputs
                                                             "out")
                                                  "/share/fonts/misc"))
                     (bdf-font-dir (string-append (assoc-ref outputs
                                                             "bdf")
                                                  "/share/fonts/misc"))
                     (otf-font-dir (string-append (assoc-ref outputs
                                                             "otf")
                                                  "/share/fonts/opentype"))
                     (pcf-font-dir (string-append (assoc-ref outputs
                                                             "pcf")
                                                  "/share/fonts/misc"))
                     (psf-font-dir (string-append (assoc-ref outputs
                                                             "psf")
                                                  "/share/consolefonts")))
                (mkdir-p otb-font-dir)
                (mkdir-p bdf-font-dir)
                (mkdir-p otf-font-dir)
                (mkdir-p pcf-font-dir)
                (mkdir-p psf-font-dir)
                (for-each (lambda (otb)
                            (install-file otb otb-font-dir))
                          (find-files "." "\\.otb$"))
                (for-each (lambda (bdf)
                            (install-file bdf bdf-font-dir))
                          (find-files "." "\\.bdf$"))
                (for-each (lambda (otf)
                            (install-file otf otf-font-dir))
                          (find-files "." "\\.otf$"))
                (for-each (lambda (pcf)
                            (install-file pcf pcf-font-dir))
                          (find-files "." "\\.pcf$"))
                (for-each (lambda (psf)
                            (install-file psf psf-font-dir))
                          (find-files "." "\\.psfu$"))) #t)))))
    (home-page "https://github.com/slavfox/Cozette")
    (synopsis "Bitmap programming font")
    (description "Cozette is a 6x13px (bounding box) bitmap font based on Dina
and heavily inspired by Creep.")
    (license license:expat)))
