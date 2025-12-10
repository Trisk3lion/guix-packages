(define-module (trisk packages gitwatch)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system copy))

(define-public gitwatch
  (package
    (name "gitwatch")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gitwatch/gitwatch/archive/refs/tags/v"
                                  version ".tar.gz"))
              (sha256
               (base32 "1vs3ddlyrpx4q1c8n1by2dk71s0w37ghgmil2lrcnk33ww7bngqi"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("gitwatch.sh" "bin/gitwatch"))))
    (inputs (list git openssh-sans-x inotify-tools))
    (home-page "https://github.com/gitwatch/gitwatch")
    (synopsis "Watching git repositories and auto-committing.")
    (description "Watching git repositories")
    (license license:expat)))
