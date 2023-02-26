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
  (let ((commit "8797e514ab68c12ed05d3af88e6baba64bf08f4d")
        (revision "2"))
    (package
      (inherit emacs)
      (name "emacs-master")
      (version (git-version "30.0.50" revision commit))
      (source
       (origin
         (inherit (package-source emacs))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-mirror/emacs.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches (search-patches "emacs-exec-path.patch"
                                  "emacs-fix-scheme-indent-function.patch"
                                  "emacs-native-comp-driver-options.patch"))
         (sha256
          (base32
           "02bklz116d6bis6xhgvma23s7p4phy2p9clnwf30h23a3wbj3qxk"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags #~'())
          #~(cons* "--with-tree-sitter" "--with-xwidgets" #$flags))))
      (inputs
       (modify-inputs (package-inputs emacs)
         (prepend sqlite webkitgtk-with-libsoup2 tree-sitter)))
      (native-inputs
       (modify-inputs (package-native-inputs emacs)
         (prepend autoconf))))))

