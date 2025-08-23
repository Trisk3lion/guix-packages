(define-module (trisk packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages rust-crates))

(define-cargo-inputs lookup-cargo-inputs
  (emacs-lsp-booster =>
                     (list rust-serde-1.0.219
                           rust-serde-json-1.0.140
                           rust-anyhow-1.0.97
                           rust-lazy-static-1.5.0
                           rust-smallvec-1.15.0
                           rust-env-logger-0.10.2
                           rust-log-0.4.27
                           rust-clap-4.5.35
                           rust-clap-verbosity-flag-2.2.3)))
