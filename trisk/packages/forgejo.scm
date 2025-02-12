(define-module (trisk packages forgejo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin))

(define-public forgejo-bin
  (package
    (name "forgejo")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://codeberg.org/forgejo/forgejo/releases/download/v" version "/forgejo-" version "-linux-amd64"))
       (file-name "forgejo")
       (sha256
        (base32 "1xgmpqlwawapqzn6kpmw093n3m1yckx2im2m4vwpps7bz3c4r0wm"))))
    (build-system binary-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chmod-to-allow-patchelf
                    (lambda _
                      (chmod "forgejo" #o755))))
       #:install-plan `(("forgejo" "bin/"))))
    (propagated-inputs (list git git-lfs bash))
    (supported-systems (list "x86_64-linux"))
    (home-page "https://forgejo.org")
    (synopsis "A self-hosted lightweight software forge.")
    (description "Forgejo is a self-hosted lightweight software forge.")
    (license license:expat)))


;; This package is based on a binary distribution. While the policy is
;; to build the software from source, this package depends on a (soft)
;; fork of https://github.com/nektos/act, which causes guix import -r
;; to add 450+ new package definitions, adding a burden to
;; maintainability.
(define-public forgejo-runner
  (package
    (name "forgejo-runner")
    (version "6.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://code.forgejo.org/forgejo/runner/releases/download/v" version "/"
             name "-" version "-linux-amd64.xz"))
       (sha256
        (base32 "055lvqi28dzh7f774hw57cdksy9xskf26fvgfzfqjrr4rbd6xdg4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;; Binary distribution: no tests.
           #:phases
           #~(modify-phases %standard-phases
               ;; No configure script.
               (delete 'configure)
               (replace 'build
                 (lambda _
                   (let ((source-name (format #f "~a-~a-linux-amd64"
                                              #$(package-name this-package)
                                              #$(package-version this-package)))
                         (target-name #$(package-name this-package)))
                     (rename-file source-name target-name)
                     (chmod target-name #o555))))
               (replace 'install
                 (lambda _
                   (install-file #$(package-name this-package)
                                 (string-append #$output "/bin")))))))
    ;; Source is a prebuilt binary.
    (supported-systems (list "x86_64-linux"))
    (home-page "https://forgejo.org")
    (synopsis "Daemon that fetches workflows to run from a Forgejo instance")
    (description "The Forgejo Runner is a daemon that fetches workflows to run from a
Forgejo instance, executes them, sends back with the logs and
ultimately reports its success or failure.

This package uses the binary release of the runner and is *not* built
from source.")
    (license license:bsd-1)))
