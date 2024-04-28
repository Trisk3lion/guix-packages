(define-module (trisk packages dict)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  ;; #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages compression))

(define-public hunspell-dict-sv
  (package
    (name "hunspell-dict-sv")
    (version "2.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://extensions.libreoffice.org/assets/downloads/z/ooo-swedish-dict-"
             (string-replace-substring version "." "-") ".oxt"))
       (sha256
        (base32 "0pzlk45l9p939vql4xq4422r24b5mds4q2982vlczhml3r36533g"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source) #t)))
       #:install-plan '(("dictionaries/sv_SE.aff" "share/hunspell/")
                        ("dictionaries/sv_SE.dic" "share/hunspell/"))))
    (inputs (list unzip))
    (synopsis "Hunspell dictionary for Swedish (sv_SE)")
    (description "This package provides a dictionary for the Hunspell
spell-checking library.")
    (home-page
     "https://extensions.libreoffice.org/en/extensions/show/swedish-spelling-dictionary-den-stora-svenska-ordlistan")
    (license (list license:gpl2 license:gpl3))))
