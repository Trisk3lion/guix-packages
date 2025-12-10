(define-module (trisk packages fonts)
  #:use-module (guix build-system font)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp))

(define-public font-merriweather
  (let ((commit "4fd88c9299009d1c1d201e7da3ff75cf1de5153a"))
    (package
      (name "font-merriweather")
      (version "1.5.2")
      ;; We prefer git-fetch since it lets us get the opentype, truetype and web
      ;; fonts all in one download. The zip archive releases separate the
      ;; opentype, truetype and web fonts into three separate archives.
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/SorkinType/Merriweather")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ndycja2jzhcfbqbm0p6ka2zl1i1pdbkf0crw2lp3pi4k89wlm29"))))
      (build-system font-build-system)
      (outputs '("out" "ttf" "woff"))
      (home-page "https://github.com/IBM/plex")
      (synopsis "Merriweather")
      (description
       "This package provides the Plex font family.  It comes in a Sans, Serif,
Mono and Sans Condensed, all with roman and true italics.  The fonts have been
designed to work well in user interface (UI) environments as well as other
mediums.")
      (license license:silofl1.1))))
