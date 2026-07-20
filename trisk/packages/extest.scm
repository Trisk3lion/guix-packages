(define-module (trisk packages extest)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages rust)
  #:use-module (trisk packages rust-crates))


(define-public extest
  (package
    (name "extest")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Supreeeme/extest")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "058sll7hx8d55j0835a9124pl52izckq8nqyxkywjal78q7mj9g1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-lib
            (lambda _
              (let ((lib (string-append #$output "/lib")))
                (with-directory-excursion "target/release"
                  (install-file "libextest.so" lib))))))))
    (inputs (cargo-inputs 'extest #:module '(trisk packages rust-crates)))
    (home-page "https://github.com/Supreeeme/extest")
    (synopsis
     "Extest - X11 XTEST Reimplementation for Steam Controller on Wayland.")
    (description
     "Extest is a drop in replacement for the X11 XTEST extension. It creates a virtual device with the uinput kernel module. It's been primarily developed for allowing the desktop functionality on the Steam Controller to work while Steam is open on Wayland.
")
    (properties
     `((upstream-name . "extest")))
    (license license:expat)))
