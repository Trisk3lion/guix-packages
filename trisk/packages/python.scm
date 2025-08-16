(define-module (trisk packages python)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages base)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu))

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
