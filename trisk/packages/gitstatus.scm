(define-module (trisk packages gitstatus)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy))

(define gitstatus-git-version
  "1.5.4")
(define gitstatus-git-hash
  "015kgndk7h260bjaffnn6s0y1nhy3aqk49cy1i9rw41jg1p82cwn")

(define-public gitstatus
  (package
    (name "gitstatus")
    (version gitstatus-git-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/romkatv/gitstatus/releases/download/v"
             version "/gitstatusd-linux-x86_64.tar.gz"))
       (sha256
        (base32 gitstatus-git-hash))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments
     `(#:install-plan '(("gitstatusd-linux-x86_64" "bin/gitstatus"))))
    (synopsis "Fastest way to check gitstatus")
    (description
     "gitstatus is a 10x faster alternative to git status and git describe. Its primary use case is to enable fast git prompt in interactive shells.")
    (home-page "https://github.com/romkatv/gitstatus")
    (license license:gpl3+)))
