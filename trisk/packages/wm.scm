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
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cmake))

(define-public vali
  (let ((commit "cfc9abf485d8ac2ed4992fdd11aab4d7fd87eedf")
        (revision "1"))
    (package
      (name "vali")
      (version "0.1.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://gitlab.freedesktop.org/emersion/vali")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "129c5fkb96kgkfdnbx1z88afafy8lrb1gdkckb9zhw25k3nyq72g"))))
      (build-system meson-build-system)
      (native-inputs (list pkg-config))
      (inputs (list json-c aml))
      (propagated-inputs (list json-c aml))
      (home-page "https://gitlab.freedesktop.org/emersion/vali")
      (synopsis "")
      (description "")
      (license license:expat))))

(define-public kanshi-vali
  (package
    (inherit kanshi)
    (name "kanshi-vali")
    (arguments
     (list #:configure-flags #~(list "-Dipc=enabled")))
    (inputs (modify-inputs inputs
              (replace "libvarlink" vali)))))
