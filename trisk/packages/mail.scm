(define-module (trisk packages mail)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages search)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public mu
  (package
    (name "mu")
    (version "1.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/djcb/mu/releases/download/v"
                           version "/mu-" version ".tar.xz"))
       (sha256
        (base32 "1jwalqmvk5s4mf7bnz7gnzh6rii7n348bsflgdvyinia0zir42vp"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config emacs-minimal gnupg ;for tests
                         texinfo))
    (inputs (list glib
                  gmime
                  guile-3.0
                  xapian
                  readline
                  python))
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
      #:imported-modules `(,@%meson-build-system-modules (guix build
                                                               emacs-utils))
      #:configure-flags #~(list (format #f "-Dguile-extension-dir=~a/lib"
                                        #$output))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-bin-references
                     (lambda _
                       (substitute* '("guile/tests/test-mu-guile.cc"
                                      ;; "mu/tests/test-mu-cmd.cc"
                                      ;; "mu/tests/test-mu-cmd-cfind.cc"
                                      "mu/tests/test-mu-query.cc")
                         (("/bin/sh")
                          (which "sh")))
                       (substitute* '("lib/tests/bench-indexer.cc"
                                      "lib/utils/mu-test-utils.cc")
                         (("/bin/rm")
                          (which "rm")))
                       (substitute* '("lib/mu-maildir.cc")
                         (("/bin/mv")
                          (which "mv")))))
                   (add-after 'install 'fix-ffi
                     (lambda _
                       (substitute* (find-files #$output "mu.scm")
                         (("\"libguile-mu\"")
                          (format #f "\"~a/lib/libguile-mu\""
                                  #$output)))))
                   (add-after 'install 'install-emacs-autoloads
                     (lambda* (#:key outputs #:allow-other-keys)
                       (emacs-generate-autoloads "mu4e"
                                                 (string-append (assoc-ref
                                                                 outputs "out")
                                                  "/share/emacs/site-lisp/mu4e")))))))
    (home-page "https://www.djcbsoftware.nl/code/mu/")
    (synopsis "Quickly find emails")
    (description
     "Mu is a tool for dealing with e-mail messages stored in the
Maildir format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
    (license license:gpl3+)))
