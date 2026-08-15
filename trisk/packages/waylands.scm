(define-module (trisk packages wayland)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)

  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages base)

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages xdisorg)

  #:use-module ((guix build-system gnu))
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((guix build-system meson) #:prefix meson:)
  #:use-module ((guix build-system python) #:prefix python:)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (guix download)
  #:use-module (guix git-download))

(define-public mew
  (package
    (name "mew")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/sewn/mew")
             (commit "e96bf181b8e5ddf8b463c43a7ac56c10cd859694")))
       (sha256
        (base32 "0yzcmgxabhchipd1km8jr864rnpvwy0c6zr2hc4677sc2g827hq4"))))
    (inputs
     (list
      pkg-config
      fcft
      pixman
      wayland
      wayland-protocols
      libxkbcommon
      coreutils))
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-after 'install 'wrap
                     (lambda _
                       (let* ((out #$output)
                              (bin-dir (string-append out "/bin"))
                              (mew-run (string-append bin-dir "/mew-run"))
                              (coreutils-bindir (string-append #$(this-package-input "coreutils") "/bin"))
                              (path-prefix (list bin-dir coreutils-bindir)))
                         (wrap-program
                             mew-run
                           `("PATH" ":" prefix
                             ,path-prefix))))))
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))))
    (home-page "https://codeberg.org/sewn/mew")
    (build-system gnu-build-system)
    (synopsis "mew is a efficient dynamic menu for Wayland, an effective port of dmenu to Wayland.")
    (description "mew is a efficient dynamic menu for Wayland, an effective port of dmenu to Wayland.")
    (license license:expat)))

(define-public wfreeze
  (package
    (name "wfreeze")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/sewn/wfreeze")
             (commit "b6fe6a54ca")))
       (sha256
        (base32 "10058n5dhmvr82hhpvppn0ak6zy60pv9yi66a3dgf08fgsgvz2p0"))))
    (inputs
     (list
      pkg-config
      wayland-protocols
      wayland))
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))))
    (home-page "https://codeberg.org/sewn/wfreeze")
    (build-system gnu-build-system)
    (synopsis "Freeze the screen, and run a command. Works well with slurp and compositors such as river and dwl.")
    (description "Freeze the screen, and run a command. Works well with slurp and compositors such as river and dwl.")
    (license license:expat)))


(define-public wlopm
  (package
    (name "wlopm")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~leon_plickat/wlopm")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1qriynl80d5r7arhkiai121l0znadi00q2c8sncnlgv33ra0kici"))))
    (build-system gnu:gnu-build-system)
    (native-inputs
     (list wayland-protocols wayland))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (synopsis "wlopm - Wayland output power management cli")
    (description "wlopm - Wayland output power management

Simple client implementing zwlr-output-power-management-v1.

wlopm is licensed under the GPLv3.")
    (license license:gpl3)
    (home-page "https://git.sr.ht/~leon_plickat/wlopm")))
