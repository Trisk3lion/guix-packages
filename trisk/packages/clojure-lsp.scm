(define-module (trisk packages clojure-lsp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression))

(define clojure-lsp-git-version
  "2023.12.29-12.09.27")
(define clojure-lsp-git-hash
  "0qwxjzyisdxc61a43df1rmk1pqifankg0g51sw6hlrjyw8sk06q5")

;; Has been upstreamed to nongnu guix
;; (define-public clojure-lsp
;;   (package
;;     (name "clojure-lsp")
;;     (version clojure-lsp-git-version)
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append
;;              "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
;;              version
;;              "/clojure-lsp-native-static-linux-amd64.zip"))
;;        (sha256
;;         (base32 clojure-lsp-git-hash))))
;;     (build-system copy-build-system)
;;     (native-inputs (list unzip))
;;     (arguments
;;      `(#:install-plan
;;        '(("clojure-lsp" "bin/"))))
;;     (home-page "https://clojure.org/releases/tools")
;;     (synopsis "CLI tools for the Clojure programming language")
;;     (description "The Clojure command line tools can be used to start a
;; Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
;;     (license license:epl1.0)))
