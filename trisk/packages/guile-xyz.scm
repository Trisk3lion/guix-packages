(define-module (trisk packages guile-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public gubar-fork
  (let ((version "0.1.0")
        (commit "6d9f0c0d213f1ad658b982dfd1c7a2450c1f2ee0")
        (revision "2"))
    (package
      (inherit gubar)
      (name "gubar-fork")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://codeberg.org/Kribbstar/gubar")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0shjvb3dcjg8rlhys4jc5xgavl50155dgv7r2hswhrymhqkjfirg")))))))

(define-public guile-ac-d-bus-latest
  (let ((commit "00547af34ea1465751f61d32f17e2915e78529c5")
        (revision "1"))
    (package
      (name "guile-ac-d-bus-latest")
      (version (string-append "1.0.0-beta." revision))
      ;; (version (git-version "0" revision commit))
      (home-page "https://gitlab.com/weinholt/ac-d-bus/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url home-page)
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1y6rfdbqzwjjn4a2sy4i3mfr15qn01flcjmycgqh10y696imxdbh"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:compile-flags #~(list "--r6rs" "-Wunbound-variable" "-Warity-mismatch")
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'adjust-for-guile
              (lambda _
                ;; Adjust source file names for Guile.
                (define (guile-sls->sls file)
                  (string-append (string-drop-right
                                  file (string-length ".guile.sls"))
                                 ".sls"))

                ;; Remove files targeting other implementations: *.mosh.sls,
                ;; etc.
                (for-each delete-file
                          (find-files
                           "compat"
                           (lambda (file stat)
                             (not (or (string-contains file ".guile.")
                                      (string-contains file "acdbuscompat.c"))))))

                ;; Rename *.guile.sls to *.sls so the ".guile" bit does not
                ;; appear in .go file names.
                (for-each (lambda (file)
                            (rename-file file (guile-sls->sls file)))
                          (find-files "compat" "\\.guile\\.sls"))

                ;; Move directories under d-bus/ to match module names.
                (mkdir "d-bus")
                (for-each (lambda (directory)
                            (rename-file directory
                                         (string-append "d-bus/"
                                                        directory)))
                          '("compat" "protocol"))
                ;; Compile libacdbuscompat.so
                (invoke #$(cc-for-target)
                        "-Wall" "-Os" "-g" "-fPIC" "-shared"
                        "-o" "libacdbuscompat.so" "d-bus/compat/acdbuscompat.c")
                (install-file "libacdbuscompat.so"
                              (string-append #$output "/lib"))
                (substitute* "d-bus/compat/socket.sls"
                  (("dynamic-link \"libacdbuscompat\"")
                   (string-append
                    "dynamic-link \""
                    (string-append #$output "/lib/libacdbuscompat.so")
                    "\"")))))
            (add-after 'build 'build-doc
              (lambda _
                (with-directory-excursion "docs"
                  (invoke "makeinfo" "ac-d-bus"))))
            (add-after 'build-doc 'check
              (lambda* (#:key (tests? #t) #:allow-other-keys)
                (when tests?
                  ;; There is no locale for the ö character, which crashes
                  ;; substitute*; reset the conversion strategy to workaround it.
                  (with-fluids ((%default-port-conversion-strategy 'substitute))
                      (substitute* (find-files "tests")
                        (("#!/usr/bin/env scheme-script")
                         (string-append "#!" (which "guile")))))
                  (invoke "./run-tests.sh"))))
            (add-after 'build-doc 'install-doc
              (lambda _
                (install-file "docs/ac-d-bus.info"
                              (string-append #$output "/share/info")))))))
      (native-inputs
       (list bash-minimal guile-3.0 texinfo gcc-toolchain))
      (propagated-inputs
       (list guile-packrat))
      (synopsis "D-Bus protocol implementation in R6RS Scheme")
      (description
       "AC/D-Bus is an implementation of the D-Bus wire protocol.  D-Bus is an
interprocess communication protocol popular on GNU/Linux systems to
communicate with a variety of services.  Originally designed for desktop
environments, it is now used by programs like VLC media player, BlueZ,
NetworkManager, Pulseaudio, systemd (including logind and resolved), Polkit,
gnome-keyring, and many more.")
      (license license:expat))))
