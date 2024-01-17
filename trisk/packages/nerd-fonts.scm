(define-module (trisk packages nerd-fonts)
  #:use-module (guix build-system font)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp))

(define NF-version "3.1.1")

(define FiraCode-hash "02b2p0gk0y998arisgknl7vmgyhxf2kfw19a3sxgfvqqbk67dmqs")
(define CommitMono-hash "0ci64628fpimb3sda2a9hi2g4ly15vkhgb9a897q8y560sv395ay")
(define iA-Writer-hash "1yd9n558fd63q8317s8my3nqn7q598zwv631k4p3g83mp9wash9r")
(define 3270-hash "196b92p74514pg6x8i09p1r841k82ci06yikvpn8ld7r6q4jx832")
(define IBMPlexMono-hash "1273jfwmyjq4l5nd9wddlmrqsjzzmab8mpa62l2aq3vnzb9gm654")
(define RobotoMono-hash "121rhk2nn24dwwmz9pqi77p6cx1gx0cz78x1fxn67f4ahfswyjp8")

(define-public FiraCode-NF
  (package
    (name "FiraCode-NF")
    (version NF-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/FiraCode.zip"))
       (sha256
        (base32 FiraCode-hash))))
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

(define-public CommitMono-NF
  (package
    (name "CommitMono-NF")
    (version NF-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/CommitMono.zip"))
       (sha256
        (base32 CommitMono-hash))))
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

(define-public iA-Writer-NF
  (package
    (name "iA-Writer-NF")
    (version NF-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/iA-Writer.zip"))
       (sha256
        (base32 iA-Writer-hash))))
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

(define-public IBMPlexMono-NF
  (package
    (name "IBMPlexMono-NF")
    (version NF-version)
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
    (version NF-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/3270.zip"))
       (sha256
        (base32 3270-hash))))
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

(define-public RobotoMono-NF
  (package
    (name "RobotoMono-NF")
    (version NF-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/RobotoMono.zip"))
       (sha256
        (base32 RobotoMono-hash))))
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
