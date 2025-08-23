(define-module (trisk packages lsp-booster)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  ;; #:use-module (trisk packages rust-crates)
  )

;; (define-public emacs-lsp-booster
;;   (package
;;     (name "emacs-lsp-booster")
;;     (version "0.2.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/blahgeek/emacs-lsp-booster")
;;              (commit (string-append "v" version))))
;;        (sha256
;;         (base32 "1xx32ms3mpi1clxf74mx7nanj0iw9qkmhi0a53fx8fkz0jw2fq8f"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:tests? #f
;;        #:install-source? #f
;;        ;; #:cargo-inputs (("rust-serde" ,rust-serde-1)
;;        ;;                 ("rust-serde-json" ,rust-serde-json-1)
;;        ;;                 ("rust-anyhow" ,rust-anyhow-1)
;;        ;;                 ("rust-lazy-static" ,rust-lazy-static-1)
;;        ;;                 ("rust-smallvec" ,rust-smallvec-1)
;;        ;;                 ("rust-env_logger" ,rust-env-logger-0.10)
;;        ;;                 ("rust-log" ,rust-log-0.4)
;;        ;;                 ("rust-clap" ,rust-clap-4)
;;        ;;                 ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2.1))
;;        ;; #:cargo-development-inputs (("rust-emacs" ,rust-emacs-0.18)
;;        ;;                             ("rust-tempfile" ,rust-tempfile-3))
;;        ;; #:phases (modify-phases %standard-phases
;;        ;;            (add-after 'unpack 'fix-version-requirements
;;        ;;              (lambda _
;;        ;;                (substitute* "Cargo.toml"
;;        ;;                  (("3.9")
;;        ;;                   ,(package-version rust-tempfile-3))) #t)))
;;        ))
;;     (inputs (cargo-inputs 'emacs-lsp-booster))
;;     (home-page "https://github.com/blahgeek/emacs-lsp-booster")
;;     (synopsis "Rust binarypp to speed up LSP in Emacs.")
;;     (description "Rust binary to speed up Eglot and lsp-mode for Emacs.")
;;     (license license:expat)))
