(define-module (trisk packages switcheroo-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages python))

(define-public switcheroo-control
  (package
    (name "switcheroo-control")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.freedesktop.org/hadess/switcheroo-control/")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "13rnl86zr77fq6ypaabp7p75y8jilqwfqjv38srqhysqy3qjizgc"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list (string-append "-Dsystemdsystemunitdir=" #$output "/share/systemd")
                                (string-append "-Dhwdbdir=" #$output "/lib/udev/hwdb.d")
                                (string-append "-Drulesdir=" #$output "/lib/udev/rules.d")
                                (string-append "-Ddatadir=" #$output "/etc"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program
                  (string-append #$output "/bin/switcherooctl")
                `("GUIX_PYTHONPATH" prefix
                  (,(string-append
                     #$(this-package-input "python-pygobject")
                     "/lib/python" #$(version-major+minor
                                      (package-version
                                       (this-package-input "python")))
                     "/site-packages")))
                ;; `("GI_TYPELIB_PATH" prefix (,(string-append
                ;;                               #$(this-package-input "glib")
                ;;                               "/lib/girepository-1.0")))
                ))))))
    (native-inputs (list pkg-config
                         cmake
                         `(,glib "bin")))
    (inputs (list libgudev
                  glib
                  python-pygobject
                  libdrm
                  python))
    (home-page "https://gitlab.freedesktop.org/hadess/switcheroo-control")
    (synopsis "")
    (description "")
    (license license:expat)))
