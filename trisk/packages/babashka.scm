(define-module (trisk packages babashka)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy))

(define babashka-git-version "1.3.189")
(define babashka-git-hash "04m7wlq2740brnpfff75005v06wr3cb751kdqrk8vfi4wz78r3i2")

(define-public babashka
  (package
   (name "babashka")
   (version babashka-git-version)
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "https://github.com/babashka/babashka/releases/download/v"
              version "/babashka-" version "-linux-amd64-static.tar.gz"))
            (sha256
             (base32 babashka-git-hash))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux" "i686-linux"))
   (arguments
    `(#:install-plan '(("bb" "bin/"))))
   (synopsis
    "Fast native Clojure scripting runtime")
   (description
    "Babashka is a native Clojure interpreter for scripting with fast startup.
Its main goal is to leverage Clojure in places where you would be using bash otherwise.")
   (home-page "https://babashka.org/")
   (license license:epl1.0)))

(define neil-git-version "0.3.65")
(define neil-git-hash "0qwxjzyisdxc61a43df1rmk1pqifankg0g51sw6hlrjyw8sk06q5")

;; (define-public neil
;;   (package
;;     (name "neil")
;;     (version neil-git-version)
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append
;;              "https://github.com/neil/neil/releases/download/"
;;              version
;;              "/neil-native-static-linux-amd64.zip"))
;;        (sha256
;;         (base32 neil-git-hash))))
;;     (build-system copy-build-system)
;;     (native-inputs (list babashka java))
;;     (arguments
;;      `(#:install-plan
;;        '(("neil" "bin/"))))
;;     (home-page "https://clojure.org/releases/tools")
;;     (synopsis "CLI tools for the Clojure programming language")
;;     (description "The Clojure command line tools can be used to start a
;; Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
;;     (license license:epl1.0)))
