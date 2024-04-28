(define-module (trisk packages lsp-booster)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics))

(define-public rust-clap-verbosity-flag-2.1
  (package
    (name "rust-clap-verbosity-flag")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-verbosity-flag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rwp3aacz3s4zlqrkiwhx83fvy66n1scfdvgz8sars6lbdgfk41w"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-clap-builder" ,rust-clap-builder-4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-strsim" ,rust-strsim-0.9)
                       ("rust-vec-map" ,rust-vec-map-0.8))))
    (home-page "https://github.com/clap-rs/clap-verbosity-flag")
    (synopsis "Easily add a `--verbose` flag to CLIs using Clap ")
    (description "Easily add a `--verbose` flag to CLIs using Clap ")
    (license (list license:expat license:asl2.0))))

(define-public emacs-lsp-booster
  (package
    (name "emacs-lsp-booster")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blahgeek/emacs-lsp-booster")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1xx32ms3mpi1clxf74mx7nanj0iw9qkmhi0a53fx8fkz0jw2fq8f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-env_logger" ,rust-env-logger-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2.1))
       #:cargo-development-inputs (("rust-emacs" ,rust-emacs-0.18)
                                   ("rust-tempfile" ,rust-tempfile-3))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-version-requirements
                    (lambda _
                      (substitute* "Cargo.toml"
                        (("3.9")
                         ,(package-version rust-tempfile-3))) #t)))))
    (home-page "https://github.com/blahgeek/emacs-lsp-booster")
    (synopsis "Rust binarypp to speed up LSP in Emacs.")
    (description "Rust binary to speed up Eglot and lsp-mode for Emacs.")
    (license license:expat)))

;; (define-public emacs-lsp-booster-mine
;;   (package
;;     (name "emacs-lsp-booster")
;;     (version "0.2.0")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/Trisk3lion/emacs-lsp-booster")
;;                     (commit "123a896ccc747d8023bb10da6d9772b085d66028")))
;;               (sha256
;;                (base32 "1kdpwi4m3kgkrmmwcawkbj45hk0vn29wjp30hmm6l3ybaf089yxq"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:tests? #f
;;        #:cargo-inputs
;;        (("rust-serde" ,rust-serde-1)
;;         ("rust-serde-json" ,rust-serde-json-1)
;;         ("rust-anyhow" ,rust-anyhow-1)
;;         ("rust-lazy-static" ,rust-lazy-static-1)
;;         ("rust-smallvec" ,rust-smallvec-1)
;;         ("rust-env_logger" ,rust-env-logger-0.10)
;;         ("rust-log" ,rust-log-0.4)
;;         ("rust-clap" ,rust-clap-4)
;;         ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2.1))
;;        #:cargo-development-inputs
;;        (("rust-emacs" ,rust-emacs-0.18)
;;         ("rust-tempfile" ,rust-tempfile-3))))
;;     (home-page "https://github.com/blahgeek/emacs-lsp-booster")
;;     (synopsis "Rust binarypp to speed up LSP in Emacs.")
;;     (description "Rust binary to speed up Eglot and lsp-mode for Emacs.")
;;     (license license:expat)))
