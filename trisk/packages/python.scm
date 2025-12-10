(define-module (trisk packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages check)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu))

(define-public python-dasbus
  (package
    (name "python-dasbus")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dasbus" version))
              (sha256
               (base32
                "1xmn6q00v3kif5q8jcq6vi84k6xb97s2ry5rgdgyxs6z3a20v1d8"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Enable tests
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pygobject))
    (home-page "https://github.com/rhinstaller/dasbus")
    (synopsis "DBus library in Python")
    (description
     "Dasbus is a DBus library written in Python 3, based on GLib and inspired
by pydbus.  It is designed to be easy to use and providing unified interface
for D-Bus.  The library is intended for creating applications that use D-Bus
for IPC and for accessing D-Bus services.

Features include:
@itemize
@item Object-oriented API
@item Support for standard D-Bus interfaces
@item Automatic type conversion
@item Proxy objects for D-Bus services
@item Support for D-Bus signals
@item Compatible with asyncio
@end itemize")
    (license license:lgpl2.1+)))

;; (define-public input-remapper
;;   (package
;;     (name "input-remapper")
;;     (version "2.2.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/sezanzeb/input-remapper")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          ;; To compute this hash:
;;          ;; guix download https://github.com/sezanzeb/input-remapper/archive/refs/tags/2.2.0.tar.gz
;;          ;; Then run: guix hash /gnu/store/...-2.2.0.tar.gz
;;          "0hpim23028008jlwwc0cymphj807dlvi23qz3iqgk6rjiz8vi4ri"))))
;;     (build-system python-build-system)
;;     (arguments
;;      (list
;;       #:tests? #f  ; Tests require dbus session and GUI environment
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (add-after 'unpack 'patch-paths
;;             (lambda* (#:key inputs #:allow-other-keys)
;;               ;; Fix FHS paths to point to store locations
;;               (substitute* "inputremapper/configs/data.py"
;;                 (("/usr/share")
;;                  (string-append #$output "/usr/share")))

;;               ;; Patch systemd service file
;;               (substitute* "data/input-remapper.service"
;;                 (("ExecStart=/usr/bin/input-remapper-service")
;;                  (string-append "ExecStart=" #$output "/bin/input-remapper-service")))

;;               ;; Patch udev rules
;;               (substitute* "data/99-input-remapper.rules"
;;                 (("RUN\\+=\"/bin/input-remapper-control")
;;                  (string-append "RUN+=\"" #$output "/bin/input-remapper-control")))

;;               ;; Patch autostart desktop file to use absolute bash path
;;               (substitute* "data/input-remapper-autoload.desktop"
;;                 (("Exec=bash")
;;                  (string-append "Exec=" (search-input-file inputs "/bin/bash"))))))

;;           (add-after 'unpack 'set-commit-hash
;;             (lambda _
;;               ;; Set the commit hash for --version output
;;               (call-with-output-file "inputremapper/commit_hash.py"
;;                 (lambda (port)
;;                   (format port "COMMIT_HASH = '~a'~%" #$version)))))

;;           ;; The setup.py already handles most installation,
;;           ;; but we need to ensure system integration files go to correct locations
;;           (add-after 'install 'install-system-files
;;             (lambda* (#:key outputs #:allow-other-keys)
;;               (let ((out (assoc-ref outputs "out")))
;;                 ;; The setup.py installs to /usr, but we need to move/install
;;                 ;; system integration files to proper Guix locations

;;                 ;; Note: setup.py already installs these, but to /usr prefix
;;                 ;; The build system should handle prefix correctly, but we ensure
;;                 ;; the files are in the right place

;;                 ;; Create directories if they don't exist
;;                 ;; (mkdir-p (string-append out "/lib/systemd/system"))
;;                 (mkdir-p (string-append out "/lib/udev/rules.d"))
;;                 (mkdir-p (string-append out "/share/polkit-1/actions"))
;;                 (mkdir-p (string-append out "/etc/dbus-1/system.d"))
;;                 (mkdir-p (string-append out "/etc/xdg/autostart"))
;;                 (mkdir-p (string-append out "/share/metainfo"))
;;                 (mkdir-p (string-append out "/share/applications"))
;;                 (mkdir-p (string-append out "/share/icons/hicolor/scalable/apps"))

;;                 ;; Install files that may not be in the right place
;;                 (unless (file-exists? (string-append out "/lib/systemd/system/input-remapper.service"))
;;                   (install-file "data/input-remapper.service"
;;                                (string-append out "/lib/systemd/system")))

;;                 (unless (file-exists? (string-append out "/lib/udev/rules.d/99-input-remapper.rules"))
;;                   (install-file "data/99-input-remapper.rules"
;;                                (string-append out "/lib/udev/rules.d")))

;;                 #t))))))

;;     (native-inputshttps://github.com/SorkinType/Merriweather
;;      (list gettext-minimal
;;            gobject-introspection
;;            pkg-config))

;;     (inputs
;;      (list bash-minimal
;;            dbus
;;            gtk+
;;            gtksourceview-4))

;;     (propagated-inputs
;;      (list python-dasbus
;;            python-evdev
;;            python-pydantic
;;            python-psutil
;;            python-pygobject
;;            python-setuptools))  ; Needed for pkg_resources at runtime

;;     (home-page "https://github.com/sezanzeb/input-remapper")
;;     (synopsis "Input device button mapping tool for Linux")
;;     (description
;;      "Input Remapper is an easy to use tool to change the mapping of your input
;; device buttons.  It supports X11, Wayland, combinations, programmable macros,
;; joysticks, wheels, triggers, keys, mouse movements and more.  It can map any
;; input to any other input.

;; Key features:
;; @itemize
;; @item Remap keyboard keys, mouse buttons, and joystick/gamepad inputs
;; @item Create complex macros with delays and repeats
;; @item Support for key combinations and sequences
;; @item Works with both X11 and Wayland
;; @item GTK-based graphical user interface
;; @item Map mouse movements and scroll wheel events
;; @item Per-device configuration presets
;; @item Automatic loading of presets on device connection
;; @item System tray integration for quick access
;; @end itemize

;; The package includes several components:
;; @itemize
;; @item @command{input-remapper-gtk} - Graphical configuration interface
;; @item @command{input-remapper-service} - Background service that applies mappings
;; @item @command{input-remapper-control} - Command-line control utility
;; @item @command{input-remapper-reader-service} - Device reader service
;; @end itemize

;; @strong{System Integration:} To use Input Remapper, you need to:
;; @enumerate
;; @item Enable the systemd service: @code{systemctl enable --now input-remapper}
;; @item Ensure your user is in the @code{input} group
;; @item Install the included udev rules, D-Bus configuration, and polkit policy
;; @end enumerate

;; On Guix System, you can add appropriate service configurations to your system
;; configuration file.  On foreign distributions, you may need to manually symlink
;; the configuration files from the package output to the appropriate system
;; locations.")
;;     (license license:gpl3+)))

(define-public python-basilisp
  (package
    (name "python-basilisp")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "basilisp" version))
       (sha256
        (base32 "19mdi18l28s7ylqrgb6w5s89q7gdsxzrb8js5asr9c8lrmqs4hnb"))))
    (properties '(("upstream-name" . "basilisp")))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs (list python-attrs
                             python-immutables
                             python-prompt-toolkit
                             python-pygments
                             python-pyrsistent
                             python-pytest
                             python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://basilisp.readthedocs.io/en/latest/")
    (synopsis "A Clojure-like lisp written for Python")
    (description
     "This package provides a Clojure-like lisp written for Python.
PYTHONPYCACHEPREFIX needs to be set to a folder outside the /gnu/store in order for $code{basilisp} to byte compile it's namespaces;
@code{export PYTHONPYCACHEPREFIX=/path/to/project/.pycache}")
    (license license:expat)))


(define-public svtplay-dl
  (package
    (name "svtplay-dl")
    (version "4.131")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "svtplay_dl" version))
       (sha256
        (base32 "03g56fy72kz1ygj4kvdg22v138ggp9blnx2r3ffmv0za4kyajwpl"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography
                             python-pysocks
                             python-pyyaml
                             python-requests))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://svtplay-dl.se")
    (synopsis
     "Command-line program to download videos from various video on demand sites")
    (description
     "Command-line program to download videos from various video on demand sites.")
    (license license:expat)))

(define-public lgpio
  (package
    (name "lgpio")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joan2937/lg")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version))
       (sha256
        (base32 "088nmmnlj6472a3g4mpzr9ag34wj8y0jhgazabw9if0cw5blnsgp"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check))
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         (string-append "DESTDIR=" (assoc-ref %outputs "out"))
         (string-append "prefix=" "")
         (string-append "PYINSTALLARGS=--prefix=" (assoc-ref %outputs "out"))
         ;; validate-runpath fails otherwise
         (string-append "LDFLAGS=-Wl,-rpath="
                        (assoc-ref %outputs "out") "/lib")
         )))
    (native-inputs (list which))
    (home-page "https://abyz.me.uk/lg/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public python-lgpio-pypi
  (package
    (name "python-lgpio-pypi")
    (version "0.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lgpio" version))
       (sha256
        (base32 "1zrzhq7ysl9ii6wm11czi9kyr1awffh272pgnfh7c3r07djjwdqi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env
            (lambda _
              (setenv "DESTDIR" (assoc-ref %outputs "out"))
              (setenv "prefix" "")
              (setenv "PYINSTALLARGS" (string-append "--prefix=" (assoc-ref %outputs "out")))
              (setenv "LDFLAGS" (string-append "-Wl,-rpath=/tmp"
                                                "/lib")))))))
    (native-inputs (list python-setuptools python-wheel swig))
    (home-page "http://abyz.me.uk/lg/py_lgpio.html")
    (synopsis "Linux SBC GPIO module")
    (description "Linux SBC GPIO module.")
    (license #f)))

(define-public python-lgpio
  (package
    (inherit lgpio)
    (name "python-lgpio")
    (native-inputs (list python python-setuptools python-wheel swig which))
    (home-page "http://abyz.me.uk/lg/py_lgpio.html")
    (synopsis "Linux SBC GPIO module")
    (description "Linux SBC GPIO module.")
    (license #f)))

(define-public python-rpi-lgpio
  (package
    (name "python-rpi-lgpio")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpi_lgpio" version))
       (sha256
        (base32 "0gmp3zbl9c5295ljvrvvi7jija6wdg6hzqf1vnyqmfs3sl8rnmw4"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs (list python-lgpio))
    (native-inputs (list python-pytest python-pytest-cov python-setuptools
                         python-wheel))
    (home-page "https://rpi-lgpio.readthedocs.io/")
    (synopsis "A compatibility shim between RPi.GPIO and lgpio")
    (description
     "This package provides a compatibility shim between RPi.GPIO and lgpio.")
    (license #f)))

(define-public python-rpi-gpio
  (package
    (name "python-rpi-gpio")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "RPi.GPIO" version))
       (sha256
        (base32 "0w1v6zxi6ixaj1z5wag03333773lcacfmkss9ax2pdip7jqc8qfd"))))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://sourceforge.net/projects/raspberry-gpio-python/")
    (synopsis "A module to control Raspberry Pi GPIO channels")
    (description
     "This package provides a module to control Raspberry Pi GPIO channels.")
    (license license:expat)))

(define-public python-spidev
  (package
    (name "python-spidev")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spidev" version))
       (sha256
        (base32 "0nqi04vvabamxxlvgj9bsrjg8giwbnc1gdis822yzam4jisw7nql"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://github.com/doceme/py-spidev")
    (synopsis "Python bindings for Linux SPI access through spidev")
    (description "Python bindings for Linux SPI access through spidev.")
    (license license:expat)))

(define-public python-keybow
  (package
    (name "python-keybow")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keybow" version))
       (sha256
        (base32 "1wlrbvdlyvm5hvn6wv4z1x0pih8kq9d5zbdvpaga5r1s38zwavsf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      ;; #:phases
      ;; #~(modify-phases %standard-phases
      ;;     (delete 'sanity-check))
      ))
    (propagated-inputs (list python-setuptools python-rpi-lgpio python-spidev))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.pimoroni.com")
    (synopsis
     "Keybow 3 and 12 key, backlit mechanical keyboard add-ons for the Raspberry Pi")
    (description
     "Keybow 3 and 12 key, backlit mechanical keyboard add-ons for the Raspberry Pi.")
    (license license:expat)))

(define-public python-bscpylgtv
  (package
    (name "python-bscpylgtv")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bscpylgtv" version))
       (sha256
        (base32 "0dazhn6lai3hwbkz3hmrx3v15iwlr143p277vyknrgxca37x5lgx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))
    (propagated-inputs (list python-sqlitedict python-websockets))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/chros73/bscpylgtv")
    (synopsis "Library to control webOS based LG TV devices.")
    (description "Library to control @code{webOS} based LG TV devices.")
    (license #f)))


;; (define-public python-winshell
;;   (package
;;     (name "python-winshell")
;;     (version "0.6")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "winshell" version ".zip"))
;;        (sha256
;;         (base32 "0h8dx820iaivca8kls1rly325r89x1klsyvh1dn1sfa3db6jz08n"))))
;;     (build-system pyproject-build-system)
;;     (arguments
;;      (list
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (delete 'sanity-check))))
;;     (native-inputs (list python-setuptools python-wheel unzip))
;;     (home-page "https://github.com/tjguk/winshell")
;;     (synopsis "Windows shell functions")
;;     (description "Windows shell functions.")
;;     (license #f)))

;; (define-public savestate
;;   (package
;;     (name "savestate")
;;     (version "1.4.3")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "https://github.com/Matteo842/SaveState/releases/download/" version "/SaveState-" version "-Linux.zip"))
;;               (sha256
;;                (base32 "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5"))))
;;     (build-system python-build-system)
;;     (propagated-inputs (list python-pyside-6
;;                              python-vdf
;;                              python-winshell))
;;     (home-page "https://github.com/Matteo842/SaveState/releases/download/1.4.3/SaveState-1.4.3-Linux.zip")
;;     (synopsis "Save files manager.")
;;     (description "Manages savefiles")
;;     (license license:expat)))
