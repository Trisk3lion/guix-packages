(define-module (trisk packages gpodder)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (trisk packages golang-xyz))



(define-public gpodder2go
  (package
    (name "gpodder2go")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Trisk3lion/gpodder2go")
             (commit "4f14fec4e19a58851930a6969e8baf63f74cfda9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "186m0qg459kn9yki4ifw1165h61bkjl2jd7iha28zzyx0k6zm4c3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/oxtyped/gpodder2go"))
    (home-page "https://github.com/oxtyped/gpodder2go")
    (synopsis "gpodder2go")
    (description
     "gpodder2go is a simple self-hosted, golang, drop-in replacement for
gpodder/mygpo server to handle podcast subscriptions management for gpodder
clients.")
    (license license:agpl3)))
