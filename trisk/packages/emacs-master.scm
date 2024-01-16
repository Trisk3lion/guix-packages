(define-module (trisk packages emacs-master)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages webkit)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (ice-9 regex)
  #:use-module (rnrs lists)
  #:use-module (trisk packages))

(define emacs-git-commit "44fcab04f6a346e602f00a6d9f5b0e6f0dbeb5e0")
(define emacs-git-hash "1bjhgl7j0wpj9wg266nka16jps36j9cgjn6fcz9d8nk0xakm6c8s")
(define emacs-git-time "1705401777")

(define-public trisk-emacs-master
  (package
    (inherit emacs-next)
    (name "trisk-emacs-master")
    (version (git-version "30.0.50" emacs-git-time emacs-git-commit))
    (source
     (origin
       (inherit (package-source emacs))
       (method url-fetch)
       (uri (string-append
             "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-" emacs-git-commit ".tar.gz"))
       (patches
        (append (search-patches  "emacs-native-comp-driver-v2.patch")
                (filter
                 (lambda (f)
                   (not (or (string-match "emacs-next-native-comp-driver-options\\.patch" f)
                            (string-match "emacs-exec-path\\.patch" f))))
                 (origin-patches (package-source emacs-next)))))
         (sha256 (base32 emacs-git-hash))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-next)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-xwidgets" #$flags))))
    (inputs
     (modify-inputs (package-inputs emacs-next)
       (prepend webkitgtk-with-libsoup2)))
    (synopsis "Emacs text editor, built from latest Git with X.")
    (description "The one and true editor!")))

(define-public trisk-emacs-pgtk
  (package
    (inherit emacs-next)
    (name "trisk-emacs-pgtk")
    (version (git-version "30.0.50" emacs-git-time emacs-git-commit))
    (source
     (origin
       (inherit (package-source emacs))
       (method url-fetch)
       (uri (string-append
             "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-" emacs-git-commit ".tar.gz"))
       (patches
        (append (search-patches "emacs-native-comp-driver-v2.patch")
                (filter
                 (lambda (f)
                   (not (or (string-match "emacs-next-native-comp-driver-options\\.patch" f)
                            (string-match "emacs-exec-path\\.patch" f))))
                 (origin-patches (package-source emacs-next-pgtk-xwidgets)))))
       (sha256 (base32 emacs-git-hash))))
    (arguments (package-arguments emacs-next-pgtk-xwidgets))
    (inputs (package-inputs emacs-next-pgtk-xwidgets))
    (synopsis "Emacs text editor, built from latest Git with --pgtk.")
    (description "The one and true editor!")))
