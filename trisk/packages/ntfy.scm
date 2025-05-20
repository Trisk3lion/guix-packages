(define-module (trisk packages ntfy)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ntfy-amd64-bin
  (package
    (name "ntfy-amd64-bin")
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/binwiederhier/ntfy/releases/download/v"
                                  version "/ntfy_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32 "1vbplsq85x6s5wjqbryhc8p0qbdll6nyi6rbdrrizqxcw9l2hwk1"))))
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
    (license license:bsd-3)))
