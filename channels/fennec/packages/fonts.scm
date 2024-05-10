(define-module (fennec packages fonts)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  )

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
    (version "1.19.3")
    (source (origin
              (method url-fetch)
	      (uri (string-append "https://github.com/slavfox/Cozette/releases/download/v."
				  version
				  "/CozetteFonts-v-"
				  (string-replace-substring version "." "-")
				  ".zip"))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w8yaf3i53pk20vvgw6bz4zj32229dc6mz210aykhpg0nk6wxwnh"))))
   (build-system font-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (source (string-append (getcwd) "/fonts"))
                   (fonts (string-append out "/share/fonts")))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/misc")))
                        (find-files source "\\.(bdf|otb)$"))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/truetype")))
                        (find-files source "\\.(ttf|ttc)$"))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/opentype")))
                        (find-files source "\\.(otf|otc)$"))
              (for-each (lambda (file)
                          (install-file file (string-append fonts "/web")))
                        (find-files source "\\.(woff|woff2)$"))
              #t))))))
    (home-page "https://github.com/slavfox/Cozette")
    (synopsis "Bitmap programming font")
    (description "Cozette is a 6x13px (bounding box) bitmap font based on Dina
and heavily inspired by Creep.")
    (license license:expat)))

;; (define-public fennec-font-intel-one-mono
;;   (package
;;     (name "fennec-font-intel-one-mono")
;;     (version "1.3.0")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/intel/intel-one-mono")
;;                     (commit (string-append "V" version))))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "0w9isn8az1k3a3q4m2llwnryy79i5v30dx1hfaf90x0zkj98ky5h"))))
;;     (outputs '("out" "ttf" "woff"))
;;     (build-system font-build-system)
;;     (arguments
;;      (list #:phases
;;            #~(modify-phases %standard-phases
;;                (add-after 'install 'split-outputs
;;                  (lambda* (#:key outputs #:allow-other-keys)
;;                    (let ((out-fonts (string-append (assoc-ref outputs "out")
;;                                                    "/share/fonts"))
;;                          (ttf-fonts (string-append (assoc-ref outputs "ttf")
;;                                                    "/share/fonts"))
;;                          (woff-fonts (string-append (assoc-ref outputs "woff")
;;                                                     "/share/fonts")))
;;                      (mkdir-p ttf-fonts)
;;                      (mkdir-p woff-fonts)
;;                      (rename-file (string-append out-fonts "/truetype")
;;                                   (string-append ttf-fonts "/truetype"))
;;                      (rename-file (string-append out-fonts "/web")
;;                                   (string-append woff-fonts "/web"))))))))
;;     (home-page "https://github.com/intel/intel-one-mono")
;;     (synopsis "Expressive monospaced font family")
;;     (description
;;      "This package provides Intel One Mono, an expressive monospaced font
;; family that's built with clarity, legibility, and the needs of developers in
;; mind.")
;;     (license license:silofl1.1)))
