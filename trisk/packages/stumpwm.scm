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
