(define-module (trisk packages nerd-fonts)
  #:use-module (guix build-system font)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp))

(define-public fira-code-NF
  (package
    (name "fira-code-NF")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/FiraCode.zip"))
       (sha256
        (base32
         "0lsk6h9f3w3wc6c5ag6s3rp58z1z6964icrn9jnwx2zggv36qbsk"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))

(define-public jet-brains-mono-NF
  (package
    (name "jet-brains-mono-NF")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/JetBrainsMono.zip"))
       (sha256
        (base32
         "1yfd5rqn4vx3irichm2h1vynpzsjccks2360npfvhi8cnzf1czwp"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))

(define-public ia-writer-NF
  (package
    (name "ia-writer-NF")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/iA-Writer.zip"))
       (sha256
        (base32
         "1nf1k644p0f4ivn0z5956d6w0ghpvv9nzw1bff89wgz8qcw76sc9"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))

(define-public ibm-plex-mono-NF
  (package
    (name "ibm-plex-mono-NF")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/IBMPlexMono.zip"))
       (sha256
        (base32
         "1s48knv0g7ksw4nmxb55f3w987f1qrjyd6l5rpy0rgsznjj515b4"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))

(define-public 3270-NF
  (package
    (name "3270-NF")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/3270.zip"))
       (sha256
        (base32
         "1khjaazf3d11mp6nnckwx1jryws47w24syf7901s14yajpcqnqc1"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license license:expat)))
