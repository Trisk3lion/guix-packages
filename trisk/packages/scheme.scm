(define-module (trisk packages scheme)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libphidget)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public loko-scheme-latest
  (package
    (name "loko-scheme-latest")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/weinholt/loko")
              (commit (string-append "v" version))))
       (sha256
        (base32 "1hmjmsli2y8wk1whcmk5xylylfrzz33hnzkwbxqwm7mslmd9dagj"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; r7rs tests are a work in progress as of 0.7.0.
      #:tests? #f
      #:strip-binaries? #f
      #:make-flags
      #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'akku-fixes
            (lambda _
              (delete-file "Akku.lock")
              (substitute* "Akku.manifest"
                (("\\(depends.*") "(depends)"))
              (invoke "akku" "install")
              (let ((dest "./.akku/lib/")
                    (source "/share/guile/site/3.0/"))
                (for-each
                 (lambda (name prefix)
                   ;; Symlink the scheme libraries so that Akku can find them
                   (symlink (string-append prefix source name)
                            (string-append dest name)))
                 '("struct" "laesare" "pfds" "machine-code")
                 (list #$(this-package-native-input "guile-struct-pack")
                       #$(this-package-native-input "guile-laesare")
                       #$(this-package-native-input "guile-pfds")
                       #$(this-package-native-input "guile-machine-code"))))
              (substitute* ".akku/env"
                (("/bin/sh") (which "sh")))
              (substitute* "scheme-wrapper"
                (("which ")
                 "command -v "))))
          (add-after 'install 'copy-libs-for-static-compile
            (lambda _
              (let ((source "./.akku/lib")
                    (dest (string-append #$output "/lib")))
                (copy-recursively source dest #:follow-symlinks? #t)))))))
    (native-inputs
     (list akku
           chez-scheme
           guile-struct-pack
           guile-machine-code
           guile-laesare
           guile-pfds))
    (home-page "https://scheme.fail")
    (synopsis "Implementation of the algorithmic language Scheme")
    (description
     "Loko Scheme is intended to be a platform for application and operating
system development.  It is written purely in Scheme and some assembler
(i.e. no C code at the bottom).  Both the R6RS and the R7RS standards are
supported.")
    (license license:agpl3+)))
