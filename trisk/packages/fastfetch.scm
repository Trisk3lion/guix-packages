(define-module (trisk packages fastfetch)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config))

(define-public fastfetch
  (package
    (name "fastfetch")
    (version "2.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fastfetch-cli/fastfetch")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0jq7xqb46spjzwji7z1hh1bcpb46lqhap7vgcc8miii21ympyjs8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/fastfetch-cli/fastfetch")
    (synopsis "Like neofetch, but much faster because written mostly in C.")
    (description "Fastfetch is a neofetch-like tool for fetching system information and displaying them in a pretty way. It is written mainly in C, with performance and customizability in mind. Currently, Linux, Android, FreeBSD, MacOS and Windows 7+ are supported.")
    (license license:expat)))
