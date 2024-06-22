(define-module (trisk packages ebook)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (guix build-system copy))


(define-public kohighlights
  (package
   (name "kohighlights")
   (version "2.0.2.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/noembryo/KoHighlights/releases/download/v" version "/KoHighlights.v" version ".PySide6.zip"))
            (sha256
             (base32 "0q7x0dz2275634gdxqm2a0d1jyni4fql5fh4abamgw3adcndzgc1"))))
   (build-system copy-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-after 'install 'make-wrapper
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (_ (mkdir-p (string-append out "/bin")))
                            (wrapper (string-append out "/bin/kohighlights")))
                       (with-output-to-file wrapper
                         (lambda _
                           (display (string-append "#!/bin/sh\n\n"
                                                   (assoc-ref inputs "python")
                                                   "/bin/python3"
                                                   " "
                                                   out
                                                   "/resources/kohighlights/main.py\n"))))
                       (chmod wrapper #o555)) #t)))
      #:install-plan '(("." "resources/kohighlights/"))))
   (inputs (list unzip))
   (propagated-inputs
    (list python
          python-beautifulsoup4
          python-future
          python-pyside-6
          python-requests
          python-packaging))
   (home-page "https://github.com/noembryo/KoHighlights")
   (synopsis "KOHighlights is a utility for viewing and exporting the Koreader's highlights to simple text, html, csv or markdown files.")
   (description "KOHighlights is a utility for viewing and exporting the Koreader's highlights to simple text, html, csv or markdown files.")
   (license license:expat)))
