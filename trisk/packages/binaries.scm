(define-module (trisk packages binaries)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ntfy-bin
  (let* ((system (or (%current-target-system) (%current-system)))
         (binary (cond
                  ((equal? system "x86_64-linux") "linux_amd64")))
         (hash (cond
                  ((equal? system "x86_64-linux") "1vbplsq85x6s5wjqbryhc8p0qbdll6nyi6rbdrrizqxcw9l2hwk1"))))
    (package
      (name "ntfy")
      (version "2.11.0")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/binwiederhier/ntfy/releases/download/v"
                                    version "/ntfy_" version "_" binary ".tar.gz"))
                (sha256
                 (base32 hash))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("ntfy" "bin/"))))
      (home-page "https://heckel.io/ntfy")
      (synopsis
       "ntfy.sh | Send push notifications to your phone or desktop via PUT/POST")
      (description
       "@@strong{ntfy} (pronounced \"\") is a simple HTTP-based
@@url{https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern,pub-sub}
notification service.  With ntfy, you can @@strong{send notifications to your
phone or desktop via scripts} from any computer, @@strong{without having to sign
up or pay any fees}.  If you'd like to run your own instance of the service, you
can easily do so since ntfy is open source.")
      (properties
       `(;(hidden? . #t)
         (release-monitoring-url . "https://github.com/binwiederhier/ntfy/releases")
         (upstream-name . "ntfy")))
      (supported-systems '("x86_64-linux"))
      (license license:bsd-3))))

;; https://github.com/henrygd/beszel/releases/download/v0.11.1/beszel-agent_linux_amd64.tar.gz
(define-public beszel-hub-bin
  (let* ((system (or (%current-target-system) (%current-system)))
         (binary (cond
                  ((equal? system "x86_64-linux") "linux_amd64")))
         (hash (cond
                  ((equal? system "x86_64-linux") "1sbirkx918a5bnm7zsyppnbqkpaz3y1q4y3c1p2ak18p04390dv3"))))
    (package
      (name "beszel")
      (version "0.11.1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/henrygd/beszel/releases/download/v"
                                    version "/" name "_" binary ".tar.gz"))
                (sha256
                 (base32 hash))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("beszel" "bin/"))))
      (home-page "https://beszel.dev")
      (synopsis "")
      (description "")
      (supported-systems '("x86_64-linux"))
      (license license:expat))))

(define-public beszel-agent-bin
  (let* ((system (or (%current-target-system) (%current-system)))
         (binary (cond
                  ((equal? system "x86_64-linux") "linux_amd64")))
         (hash (cond
                  ((equal? system "x86_64-linux") "0pri0985k754j1z3vmz7v42mz895p871p0l0was1jvgkvvlwmcmr"))))
    (package
      (name "beszel-agent")
      (version "0.11.1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/henrygd/beszel/releases/download/v"
                                    version "/" name "_" binary ".tar.gz"))
                (sha256
                 (base32 hash))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("beszel-agent" "bin/"))))
      (home-page "https://beszel.dev")
      (synopsis "")
      (description "")
      (supported-systems '("x86_64-linux"))
      (license license:expat))))


(define babashka-git-version
  "1.3.189")
(define babashka-git-hash
  "04m7wlq2740brnpfff75005v06wr3cb751kdqrk8vfi4wz78r3i2")

(define-public babashka
  (package
    (name "babashka")
    (version babashka-git-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/babashka/babashka/releases/download/v"
             version "/babashka-" version "-linux-amd64-static.tar.gz"))
       (sha256
        (base32 babashka-git-hash))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments
     `(#:install-plan '(("bb" "bin/"))))
    (synopsis "Fast native Clojure scripting runtime")
    (description
     "Babashka is a native Clojure interpreter for scripting with fast startup.
Its main goal is to leverage Clojure in places where you would be using bash otherwise.")
    (home-page "https://babashka.org/")
    (properties
       `(;(hidden? . #t)
         (release-monitoring-url . "https://github.com/babashka/babashka/releases")
         (upstream-name . "babashka")))
    (license license:epl1.0)))

(define neil-git-version
  "0.3.65")
(define neil-git-hash
  "0qwxjzyisdxc61a43df1rmk1pqifankg0g51sw6hlrjyw8sk06q5")

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
