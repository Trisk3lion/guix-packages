(define-module (trisk packages asus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  ;; #:use-module (gnu packages bison)
  ;; #:use-module (gnu packages flex)
  ;; #:use-module (gnu packages base)
  ;; #:use-module (gnu packages ocaml)
  ;; #:use-module (gnu packages perl)
  ;; #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages rust)
  #:use-module (trisk packages rust-crates))


;; (define-public asusctl
;;   (package
;;     (name "asusctl")
;;     (version "")
;;     (source origin...)
;;     (build-system gnu-build-system)
;;     (native-inputs (list pkg-config))
;;     (inputs (cons* openssl
;;                    `(,zstd "lib")
;;                    (cargo-inputs 'mod-installer #:module '(trisk packages rust-crates))))
;;     (home-page "")
;;     (synopsis "")
;;     (description "")
;;     (license license:unknown)))


    ;; install -Dm444 -t $out/lib/udev/rules.d/ data/*.rules
    ;; install -Dm444 -t $out/share/dbus-1/system.d/ data/org.supergfxctl.Daemon.conf
    ;; install -Dm444 -t $out/lib/systemd/system/ data/supergfxd.service

(define-public supergfxctl
  (package
    (name "supergfxctl")
    (version "5.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/asus-linux/supergfxctl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hxjla3yicdx9ysiwg61yl4npa416qpfnpxyh91i27985vicsy3p"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:tests? #f
           #:rust rust-1.88
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-files
                 (let ((out (assoc-reg outputs "out"))
                       ;; (stage (string-append "src/" import-path "/build/stage"))
                       )
                   (install-file "data/90-supergfxd-nvidia-pm.rules" (string-append out "/lib/udev/rules.d/"))
                   (install-file "data/99-nvidia-ac.rules" (string-append out "/lib/udev/rules.d/"))
                   (install-file "data/org.supergfxctl.Daemon.conf" (string-append out "/share/dus-1/system.d"))
                   (install-file "data/supergfxd.service") (string-append out "/lib/systemd/system/"))))))
    (native-inputs (list pkg-config))
    (inputs (cons* eudev
                   (cargo-inputs 'supergfxctl #:module '(trisk packages rust-crates))))
    (propagated-inputs (list pciutils kmod))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))
