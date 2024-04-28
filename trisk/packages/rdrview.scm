(define-module (trisk packages rdrview)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml))

(define-public rdrview
  (package
    (name "rdrview")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/eafer/rdrview/archive/refs/tags/v" version
             ".tar.gz"))
       (sha256
        (base32 "0qrzvzbbhazfp2gc4c4gjindlraj8jza6wkgrydbh32gsxzxfma6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;Test suite needs work.
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure) ;no configure script.
                  (add-before 'check 'pre-check
                    (lambda _
                      (setenv "HOME"
                              (getcwd))
                      (with-output-to-file ".mailcap"
                        (lambda ()
                          (display
                           "text/html;      links -dump %s; copiousoutput
")))
                      #t))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (with-directory-excursion "tests"
                            (invoke "./check" "-V")) #t))))))
    ;; (native-inputs
    ;; `(("links" ,(@ (gnu packages web-browsers) links))
    ;; ("tidy" ,(@ (gnu packages web) tidy))
    ;; ("valgrind" ,(@ (gnu packages valgrind) valgrind))))
    (inputs `(("curl" ,curl)
              ("libseccomp" ,libseccomp)
              ("libxml2" ,libxml2)))
    (home-page "https://github.com/eafer/rdrview")
    (synopsis "Extract the main content from a webpage")
    (description
     "Command line tool to extract the main content from a
webpage, as done by the \"Reader View\" feature of most modern browsers.  It's
intended to be used with terminal RSS readers, to make the articles more
readable on web browsers such as @code{lynx}.  The code is closely adapted from
the @url{https://github.com/mozilla/readability, Firefox version} and the
output is expected to be mostly equivalent.")
    (license license:asl2.0)))
