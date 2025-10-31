(define-module (trisk packages owntracks)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages linux))


;; Från nix paketet:

    ;; cp config.mk.in config.mk

    ;; substituteInPlace config.mk \
    ;;   --replace "INSTALLDIR = /usr/local" "INSTALLDIR = $out" \
    ;;   --replace "DOCROOT = /var/spool/owntracks/recorder/htdocs" "DOCROOT = $out/htdocs" \
    ;;   --replace "WITH_LUA ?= no" "WITH_LUA ?= yes" \
    ;;   --replace "WITH_ENCRYPT ?= no" "WITH_ENCRYPT ?= yes"


(define-public owntracks-recorder
  (package
    (name "owntracks-recorder")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/owntracks/recorder/archive/refs/tags/"
                                  version ".tar.gz"))
              (sha256
               (base32 "1ikgk3iax5h05aark2py0g9cij0kncj9s4izxmwi0hfb97p40w04"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list "CC=gcc"
                           (string-append "DESTDIR=" #$output)
                           "INSTALLDIR="
                           (string-append "DOCROOT=/share/htdocs"))
      ;; #:modules '((guix build gnu-build-system)
      ;;             (guix build utils))
      ;; Lua provides no .pc file, so add CFLAGS/LIBS manually.
     ;; #:configure-flags
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)

          (add-before 'build 'configure-config
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (copy-file "config.mk.in" "config.mk")
                (substitute* "config.mk"
                  (("TZDATADB = /usr/share/owntracks/recorder/timezone16.bin")
                   "TZDATADB = /var/lib/owntracks/recorder/timezone16.bin")
                  (("CONFIGFILE = /etc/default/ot-recorder"
                    "CONFIGFILE = /etc/ot-recorder"))
                  (("WITH_LUA \\?= no")
                   "WITH_LUA ?= no")
                  (("WITH_ENCRYPT \\?= no")
                   "WITH_ENCRYPT ?= yes"))))))))
    (native-inputs (list pkg-config))
    (inputs (list mosquitto
                  curl
                  openssl
                  lua
                  libsodium
                  lmdb
                  libconfig
                  `(,util-linux "lib") ;; uuid
                  ))
    (home-page "https://github.com/owntracks/recorder")
    (synopsis "The OwnTracks Recorder is a lightweight program for storing and accessing location data published via MQTT (or HTTP) by the OwnTracks apps.")
    (description "The OwnTracks Recorder is a lightweight program for storing and accessing location data published via MQTT (or HTTP) by the OwnTracks apps. It is a compiled program which is easy to install and operate even on low-end hardware, and it doesn't require an external database.
There are two main components: the Recorder obtains data via MQTT subscribes or HTTP POST, stores the data in plain files and serve it via its built-in REST API, and the ocat command-line utility reads stored data in a variety of formats.

We developed the Recorder as a one-stop solution to storing location data published by our OwnTracks apps (iOS and Android) and retrieving this data. ")
    (license license:expat)))
