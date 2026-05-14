(define-module (trisk packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages c)
  #:use-module (gnu packages wm))

(define-public vali
  (package
    (name "vali")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.freedesktop.org/emersion/vali")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0xycciy9f2iz41w8vrj0w0pan75x3hfgb15g67262kw7ga5lkfxv"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list json-c aml))
    (home-page "https://gitlab.freedesktop.org/emersion/vali")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public kanshi-vali
  (package
    (inherit kanshi)
    (name "kanshi-vali")
    (arguments
     (list
      #:parallel-build? #f
      #:configure-flags #~(list "-Dipc=enabled")))
    (inputs (modify-inputs inputs
              (replace "libvarlink" vali)))))

(define-public kanshi-1.8.0
  (package
    (inherit kanshi)
    (name "kanshi-1.8.0")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.freedesktop.org/emersion/kanshi")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aw43fdrrgywghy59ahfxwq0613ydkpl2j6p042c0iwqv1b6fhgp"))))))
