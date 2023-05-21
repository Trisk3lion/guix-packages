(define-module (trisk packages emacs-master)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (ice-9 regex)
  #:use-module (rnrs lists)
  #:use-module (trisk packages))

(define-public trisk-emacs-master
  (let ((commit "f8cdb9e05067fba5dee193ac604e75a67a7ff2c9")
        (revision "0"))
    (package
      (inherit emacs-next-tree-sitter)
      (name "trisk-emacs-master")
      (version (git-version "30.0.50" revision commit))
      (source
       (origin
         (inherit (package-source emacs-next-tree-sitter))
         (patches
          (append (search-patches "emacs-native-comp-driver-v2.patch")
                  (filter
                   (lambda (f)
                     (not (string-match "emacs-native-comp-driver-options\\.patch" f)))
                   (origin-patches (package-source emacs-next-tree-sitter)))))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-mirror/emacs.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0587ljnqgy5l9k53n1ls8yvabf08krgkbh85qdzp671nw3lj05mr"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next-tree-sitter)
         ((#:configure-flags flags #~'())
          #~(cons* "--with-xwidgets" #$flags))))
      (inputs
       (modify-inputs (package-inputs emacs-next-tree-sitter)
         (prepend webkitgtk-with-libsoup2 libxcomposite)))
      (synopsis "Emacs text editor, built from latest Git with X.")
      (description "The one and true editor!"))))

(define-public trisk-emacs-pgtk
  (let ((commit "f8cdb9e05067fba5dee193ac604e75a67a7ff2c9")
        (revision "0"))
    (package
      (inherit emacs-next-pgtk)
      (name "trisk-emacs-pgtk")
      (version (git-version "30.0.50" revision commit))
      (source
       (origin
         (inherit (package-source emacs-next-pgtk))
         (patches
          (append (search-patches "emacs-native-comp-driver-v2.patch")
                  (filter
                   (lambda (f)
                     (not (string-match "emacs-native-comp-driver-options\\.patch" f)))
                   (origin-patches (package-source emacs-next-pgtk)))))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-mirror/emacs.git")
               (commit commit)))
         (sha256
          (base32
           "0587ljnqgy5l9k53n1ls8yvabf08krgkbh85qdzp671nw3lj05mr"))))
      (synopsis "Emacs text editor, built from latest Git with --pgtk.")
      (description "The one and true editor!"))))
