(define-module (trisk packages lsp-booster)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (trisk packages rust-crates)
  )


;; Generera declaration för beroenden:
;; guix import --insert=trisk/packages/rust-crates.scm \
;;     crate --lockfile=/path/to/Cargo.lock PACKAGE
;; Generera paket deklaration:
;; guix import crate PACKAGE


(define-public emacs-lsp-booster
  (package
    (name "emacs-lsp-booster")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "emacs-lsp-booster" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j0ayr0sdanssbv2c4kprzxfn2m3ihk7h5kzvgh7m8wavxv9wc0k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f))
    (inputs (cargo-inputs 'emacs-lsp-booster))
    (home-page "https://github.com/blahgeek/emacs-lsp-booster")
    (synopsis "Emacs LSP performance booster")
    (description "This package provides Emacs LSP performance booster.")
    (license license:expat)))
