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
      (name "ntfy-bin")
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
