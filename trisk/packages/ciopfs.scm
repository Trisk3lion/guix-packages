(define-module (trisk packages ciopfs)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages attr)
  #:use-module ((guix licenses)
                #:prefix license:))


(define-public ciopfs
  (package
    (name "ciopfs")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/martanne/ciopfs/releases/download/v"
             version "/ciopfs-" version ".tar.gz"))
       (sha256
        (base32 "0sr9i9b3qfwbfvzvk00yrrg3x2xqk1njadbldkvn7hwwa4z5bm9l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          "PREFIX="
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       ;; #~(list (string-append "CC=" #$(cc-for-target))
       ;;             (string-append "DESTDIR=" #$output)
       ;;             "PREFIX=''")
       #:phases (modify-phases %standard-phases
                               (add-after 'unpack 'set-file-names
                                          (lambda* (#:key inputs outputs #:allow-other-keys)
                                            (substitute* "Makefile"
                                                         (("@ln -sf \\$\\{PREFIX\\}/bin/ciopfs") "@ln -sf ${DESTDIR}/bin/ciopfs"))))
                              (delete 'configure))))
    (native-inputs (list pkg-config))
    (inputs (list fuse-2 glib attr))
    (synopsis "Case-insensitive FUSE file system,")
    (description "Ciopfs is a fuse file system that mounts a directory on a regular file system in case insensitive fashion. ")
    (home-page "https://github.com/martanne/ciopfs")
    (license license:gpl3+)))
