(define-module (trisk packages stumpwm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system emacs)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm))


(define-public sbcl-stumpwm-stumpbuffer
  (let ((commit "a7158765b9e31deba50a1147ded8776a002565dd")
        (revision "0"))
    (package
      (name "sbcl-stumpwm-stumpbuffer")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/juki-pub/stumpbuffer/")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gkxrwjrkfz4rykk6nndkmigqdrihmvnq58m9r1177zp54bhs9vd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-systems '("stumpbuffer")
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "cl/stumpbuffer"))))))
      (inputs (list stumpwm))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "Keyboard-driven divide-and-conquer mouse control mode")
      (description "This package provides a keyboard-driven divide-and-conquer
mouse control mode for StumpWM.")
      (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))

(define-public emacs-stumpbuffer
  (let ((commit "a7158765b9e31deba50a1147ded8776a002565dd")
        (revision "0"))
  (package
    (name "emacs-stumpbuffer")
    (version (git-version "0.0.1" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/juki-pub/stumpbuffer/")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gkxrwjrkfz4rykk6nndkmigqdrihmvnq58m9r1177zp54bhs9vd"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f
       #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "emacs/stumpbuffer"))))))
    (inputs (list stumpish))
    (home-page "https://github.com/xcwen/ac-php")
    (synopsis "Emacs Auto Complete & Company mode for PHP")
    (description
     "This package provides Auto Complete and Company back-ends for PHP.")
    (license license:gpl3+))))

(define stumpwm-contrib
  (let ((commit "c4f077b1fe97cd8da6d710e5cbe390eb680629bd")
        (revision "7"))
    (package
      (name "stumpwm-contrib")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fdv4d0rlca64p4dakp1l60701vls2s6kx3gzlflmcf2l49kdbnn"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list stumpwm))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "StumpWM extra modules")
      (description "This package provides extra modules for StumpWM.")
      (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))

(define-public sbcl-stumpwm-mpd
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-mpd")
    (arguments
     '(#:asd-systems '("mpd")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "minor-mode/mpd"))))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Minor-mode for controlling mpd")
    (description "This package provides a keyboard-driven divide-and-conquer
mouse control mode for StumpWM.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))
