(define-module (trisk packages binaries)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ntfy-bin
  (let* ((system (or (%current-target-system) (%current-system)))
         (binary (cond
                  ((equal? system "x86_64-linux") "linux_amd64")))
         (hash (cond
                  ((equal? system "x86_64-linux") "13jg1hk5z2bzw8jaw0yb3c6q8390ic0407945dw4f9d7l1im56h1"))))
    (package
      (name "ntfy")
      (version "2.15.0")
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
       '(;(hidden? . #t)
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
      (version "0.17.0")
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
                  ((equal? system "x86_64-linux") "0vhb242qb4p77pcs5xm5s4k0hbyf7h5h28yw1k8qiq8srlaxp78j"))))
    (package
      (name "beszel-agent")
      (version "0.17.0")
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


;; https://github.com/editor-code-assistant/eca/releases/download/0.32.4/eca-native-static-linux-amd64.zip
(define-public editor-code-assistant
  (package
    (name "editor-code-assistant")
    (version "0.90.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/editor-code-assistant/eca/"
                           "/releases/download/" version
                           "/eca-native-static-linux-amd64.zip"))
       (sha256
        (base32 "0qp5dwwxlyvbylkn7n10csl3naf2jqi1lxxv2fyc47b6szm0hwlk"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("./eca" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chmod
           (lambda _
             (chmod "./eca" #o755))))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/clojure-lsp/clojure-lsp")
    (synopsis "Clojure & ClojureScript Language Server (LSP) implementation")
    (description "This package provides a Language Server for Clojure and ClojureScript
languages.  The goal of this project is to bring great editing tools for
Clojure/Clojurescript to all editors and programatically via its CLI and API.
It aims to work alongside you to help you navigate, identify and fix errors,
perform refactors and more.")
    (properties
     '((release-monitoring-url . "https://github.com/editor-code-assistant/eca/releases")
       (upstream-name . "eca")))
    (license license:expat)))
