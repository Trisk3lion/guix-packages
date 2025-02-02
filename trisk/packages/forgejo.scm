(define-module (trisk packages forgejo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin))

(define-public forgejo-bin
  (package
    (name "forgejo")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://codeberg.org/forgejo/forgejo/releases/download/v" version "/forgejo-" version "-linux-amd64"))
       (file-name "forgejo")
       (sha256
        (base32 "1xgmpqlwawapqzn6kpmw093n3m1yckx2im2m4vwpps7bz3c4r0wm"))))
    (build-system binary-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chmod-to-allow-patchelf
                    (lambda _
                      (chmod "forgejo" #o755))))
       #:install-plan `(("forgejo" "bin/"))))
    (propagated-inputs (list git git-lfs bash))
    (home-page "https://forgejo.org")
    (synopsis "A self-hosted lightweight software forge.")
    (description "Forgejo is a self-hosted lightweight software forge.")
    (license license:expat)))
