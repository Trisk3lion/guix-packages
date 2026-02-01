(define-module (trisk packages hare-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system hare)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages man))

(define-public powerctl
  (package
    (name "powerctl")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~sircmpwn/powerctl")
                    (commit "78e1dab458fdf581c03123be6bf6e5f0c8f22876")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "175qq23198n6jzvwyir9k5vdw70ys2kzk8hvmg5a3mkn7150n4dp"))))
    (build-system hare-build-system)
    (arguments (list #:tests? #f))
    (supported-systems %hare-supported-systems)
    (inputs (list scdoc))
    (home-page "https://git.sr.ht/~sircmpwn/powerctl")
    (synopsis "A simple command line utility to control power states on Linux, i.e. to suspend or hibernate the system.")
    (description "A simple command line utility to control power states on Linux, i.e. to suspend or hibernate the system.")
    (license license:expat)))
