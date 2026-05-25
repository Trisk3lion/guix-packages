(define-module (trisk packages guile-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public gubar-fork
  (let ((version "0.1.0")
        (commit "8c25dac1e09d7a85531fa9c86e4a2740ed01202e")
        (revision "1"))
    (package
      (inherit gubar)
      (name "gubar-fork")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/Kribbstar/gubar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0vg249599g7d08bxi4gmm363nnm7x1hwm7ijjckav2y2av1ykvcg")))))))
