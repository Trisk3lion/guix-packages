(define-module (trisk packages emacs-master)
  #:use-module (gnu packages)   ;; search-patches
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages autotools)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (ice-9 regex))

(define-public trisk-emacs-master
  (let ((commit "9b66a64d9c2c7ae2b155bf209ad735383070822e")
        (revision "0"))
    (package
      (inherit emacs-next)
      (name "trisk-emacs-master")
      (version (git-version "30.0.50" revision commit))
      (source
       (origin
         (inherit (package-source emacs-next))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-mirror/emacs.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches (search-patches "emacs-exec-path.patch"
                                "emacs-fix-scheme-indent-function.patch"))
         (sha256
          (base32
           "1drc0x5ib9irhi7myp9lmjzhwl5ibdwrs3lxbsg07kh8n2dgmkhg"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-next)
         ((#:configure-flags flags #~'())
          #~(cons* "--with-tree-sitter" "--with-xwidgets" #$flags))))
      (inputs
       (modify-inputs (package-inputs emacs-next)
         (prepend sqlite webkitgtk-with-libsoup2 tree-sitter)))
      (native-inputs
       (modify-inputs (package-native-inputs emacs-next)
         (prepend autoconf)))
      (synopsis "Emacs text editor, built from latest Git.")
      (description "The one and true editor!"))))
