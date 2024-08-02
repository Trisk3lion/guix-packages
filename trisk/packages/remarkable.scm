(define-module (trisk packages remarkable)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages syncthing)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system go))

;; d07b7360a2ef5914c72194395850731462b7b520
(define-public remy
  (let ((version "0.0.0")
        (revision "0")
        (commit "df2c1aec2efe8ba8f1f2ae098478eb8f580188dd"))
   (package
    (name "remy")
    (version (git-version version revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bordaigorl/remy")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x2n58q8gpfkmwxqk0zrv069diamigcs35fn99lfn98412hqvdwi"))))
    (build-system python-build-system)
    (propagated-inputs (list python-arrow
                             python-paramiko
                             python-pypdf2
                             python-pyqt
                             python-requests
                             python-sip))
    (home-page "https://github.com/bordaigorl/remy")
    (synopsis "Online&offline manager for the reMarkable tablet")
    (description "The goal of Remy is to allow simple interaction with the
reMarkable tablet over ssh, without needing the cloud service, nor the USB Web
UI.")
    (license license:gpl3+))))

(define-public go-github-com-abiosoft-readline
  (package
    (name "go-github-com-abiosoft-readline")
    (version "0.0.0-20180607040430-155bce2042db")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abiosoft/readline")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104q8dazj8yf6b089jjr82fy9h1g80zyyzvp3g8b44a7d8ngjj6r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abiosoft/readline"))
    (propagated-inputs (list go-github-com-chzyer-test
                             go-github-com-chzyer-logex))
    (home-page "https://github.com/abiosoft/readline")
    (synopsis "Guide")
    (description
     "Readline is a pure go implementation for GNU-Readline kind library.")
    (license license:expat)))

(define-public go-github-com-abiosoft-ishell
  (package
    (name "go-github-com-abiosoft-ishell")
    (version "0.0.0-20210602093954-1c52a1a9f803")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abiosoft/ishell")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jmnnszqni4nmrpygpxgb8j4v76ipsj4g4hv0zl9sccv2zarixp9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abiosoft/ishell"))
    (propagated-inputs (list go-github-com-stretchr-testify
                         go-github-com-flynn-archive-go-shlex
                         go-github-com-fatih-color
                         go-github-com-abiosoft-readline))
    (home-page "https://github.com/abiosoft/ishell")
    (synopsis "ishell")
    (description "Package ishell implements an interactive shell.")
    (license license:expat)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "0.0.0-20180221191011-83c6a9932646")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nfnt/resize")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "005cpiwq28krbjf0zjwpfh63rp4s4is58700idn24fs3g7wdbwya"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nfnt/resize"))
    (home-page "https://github.com/nfnt/resize")
    (synopsis
     "This package is no longer being updated! Please look for alternatives if that bothers you.")
    (description "Package resize implements various image resizing methods.")
    (license license:isc)))

(define-public go-github-com-adrg-strutil
  (package
    (name "go-github-com-adrg-strutil")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adrg/strutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkjzjllv8b2m3lgn66cb09b0f5xqy2bk8ny3lkn4z0ywlchawj9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/strutil"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/adrg/strutil")
    (synopsis "Installation")
    (description
     "Package strutil provides string metrics for calculating string similarity as
well as other string utility functions.  Documentation for all the metrics can
be found at
@@url{https://pkg.go.dev/github.com/adrg/strutil/metrics,https://pkg.go.dev/github.com/adrg/strutil/metrics}.")
    (license license:expat)))

(define-public go-github-com-adrg-xdg
  (package
    (name "go-github-com-adrg-xdg")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adrg/xdg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xbkb8wmr6phj2ppr75akc58jdzrv20gc3mkxa1mmb968isy8s6c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/xdg"
      ;; Include test that tries to create directories
      #:tests? #f))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/adrg/xdg")
    (synopsis "Installation")
    (description
     "Package xdg provides an implementation of the XDG Base Directory Specification.
The specification defines a set of standard paths for storing application files
including data and configuration files.  For portability and flexibility
reasons, applications should use the XDG defined locations instead of hardcoding
paths.  The package also includes the locations of well known user directories.")
    (license license:expat)))

(define-public go-github-com-adrg-sysfont
  (package
    (name "go-github-com-adrg-sysfont")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adrg/sysfont")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h55mnjikyknmbajahww8h7i76zx2ngjdfnwbqsh8dzx8ra1ydm5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/adrg/sysfont"))
    (propagated-inputs (list go-github-com-adrg-xdg
                             go-github-com-adrg-strutil))
    (home-page "https://github.com/adrg/sysfont")
    (synopsis "sysfont")
    (description
     "Package sysfont is a small package that makes it easy to identify installed
fonts.  It is useful for listing installed fonts or for matching fonts based on
user queries.  The matching process also suggests viable font alternatives.")
    (license license:expat)))

(define-public go-github-com-boombuler-barcode
  (package
    (name "go-github-com-boombuler-barcode")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/boombuler/barcode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v4ypgh3xarzfpgys838mgkfabqacbjklhf4kfqnycs0v0anvnlr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/boombuler/barcode"))
    (home-page "https://github.com/boombuler/barcode")
    (synopsis "Introduction")
    (description
     "This is a package for GO which can be used to create different types of
barcodes.")
    (license license:expat)))

(define-public go-github-com-gorilla-i18n
  (package
    (name "go-github-com-gorilla-i18n")
    (version "0.0.0-20150820051429-8b358169da46")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/i18n")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10wx2djn6lbj6c9q1yx3gbz510hcnhvb4s9vsdq8cm6jriv11r7m"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; #:go go-1.4
      #:import-path "github.com/gorilla/i18n"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Source-only package.
          (delete 'build))))
    (home-page "https://github.com/gorilla/i18n")
    (synopsis "i18n")
    (description
     "gorilla/i18n groups packages related to internationalization")
    (license license:bsd-3)))

(define-public go-github-com-montanaflynn-stats
  (package
    (name "go-github-com-montanaflynn-stats")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/montanaflynn/stats")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y38cvp7r6fb6291k82j781dbykx00mxw8ca0v9d0fijzc1x81fi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/montanaflynn/stats"))
    (home-page "https://github.com/montanaflynn/stats")
    (synopsis "Stats - Golang Statistics Package")
    (description
     "Package stats is a well tested and comprehensive statistics library package with
no dependencies.")
    (license license:expat)))

(define-public go-github-com-trimmer-io-go-xmp
  (package
    (name "go-github-com-trimmer-io-go-xmp")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trimmer-io/go-xmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajcnlfllm4w9x78n91zbhybq8sa28yjwj7i5lsyaz0ahn1z5g9x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.17
      #:import-path "github.com/trimmer-io/go-xmp"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Source-only package.
          (delete 'build))))
    (propagated-inputs (list go-github-com-montanaflynn-stats
                             go-github-com-golang-snappy))
    (home-page "https://github.com/trimmer-io/go-xmp")
    (synopsis "go-xmp")
    (description
     "go-xmp is a native @@url{http://golang.org/,Go} SDK for the
@@url{http://www.adobe.com/devnet/xmp.html,Extensible Metadata Platform} (XMP)
as defined by the Adobe XMP Specification
@@url{http://wwwimages.adobe.com/content/dam/Adobe/en/devnet/xmp/pdfs/XMP%20SDK%20Release%20cc-2016-08/XMP@code{SpecificationPart1.pdf,Part}
1},
@@url{http://wwwimages.adobe.com/content/dam/Adobe/en/devnet/xmp/pdfs/XMP%20SDK%20Release%20cc-2016-08/XMP@code{SpecificationPart2.pdf,Part}
2} and
@@url{http://wwwimages.adobe.com/content/dam/Adobe/en/devnet/xmp/pdfs/XMP%20SDK%20Release%20cc-2016-08/XMP@code{SpecificationPart3.pdf,Part}
3}, a.k.a ISO 16684-1:2011(E).")
    (license license:asl2.0)))

(define-public go-github-com-unidoc-freetype
  (package
    (name "go-github-com-unidoc-freetype")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/freetype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mrspyf96vx20v988nkdmk8zcwifwhglhp68ha23jm5q58ms8i0j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/freetype"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Source-only package.
          (delete 'build)
          (add-after 'unpack 'fix-strange-import
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/raster/raster.go")
                (("// import \"github.com/golang/freetype/raster\"")
                 ""))
              (substitute* (string-append "src/" import-path
                                          "/truetype/truetype.go")
                (("// import \"github.com/golang/freetype/truetype\"")
                 "")))))))
    (propagated-inputs (list go-golang-org-x-image))
    (home-page "https://github.com/unidoc/freetype")
    (synopsis #f)
    (description
     "The freetype package provides a convenient API to draw text onto an image.  Use
the freetype/raster and freetype/truetype packages for lower level control over
rasterization and @code{TrueType} parsing.")
    (license license:freetype)))

(define-public go-github-com-unidoc-garabic
  (package
    (name "go-github-com-unidoc-garabic")
    (version "0.0.0-20220702200334-8c7cb25baa11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/garabic")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mvxkjgpk0j83zlkxnhq77jibfabyyrrbx8zsgbccxipp8yjym7s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/garabic"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/unidoc/garabic")
    (synopsis "GArabic")
    (description
     "Package garabic provides a set of functions for Arabic text processing in
golang.")
    (license license:asl2.0)))

(define-public go-github-com-unidoc-pkcs7
  (package
    (name "go-github-com-unidoc-pkcs7")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/pkcs7")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h0hlvjk888jp4x79cd7c7cmji5lway21bq6dkw63rw9zi2qcdyp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/pkcs7"
      #:tests? #f ;; TODO Needs openssl for tests
      ))
    (home-page "https://github.com/unidoc/pkcs7")
    (synopsis "pkcs7")
    (description
     "Package pkcs7 implements parsing and generation of some PKCS#7 structures.")
    (license license:expat)))

(define-public go-github-com-unidoc-timestamp
  (package
    (name "go-github-com-unidoc-timestamp")
    (version "0.0.0-20200412005513-91597fd3793a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/timestamp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09xyjqfzp2k62i173v1aar40279411jl8adpqkxmn6p76aw4f8hi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/timestamp"
      #:tests? #f ;; Test are failing for unknown reason
      ))
    (propagated-inputs (list go-github-com-unidoc-pkcs7))
    (home-page "https://github.com/unidoc/timestamp")
    (synopsis "RFC3161 Time-Stamp Protocol (TSP) package for Go")
    (description
     "Package timestamp implements the Time-Stamp Protocol (TSP) as specified in
RFC3161 (Internet X.509 Public Key Infrastructure Time-Stamp Protocol (TSP)).")
    (license license:bsd-2)))

(define-public go-github-com-unidoc-unichart
  (package
    (name "go-github-com-unidoc-unichart")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/unichart")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10w5xfalljzaz880dgfr5sw0r7aybxqbc905dwj5vbld5116s3qm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/unichart"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/unidoc/unichart")
    (synopsis "unichart")
    (description
     "unichart is a native Golang charting library.  Its primary purpose is to
integrate with @@url{https://github.com/unidoc/unipdf,@code{UniPDF}} and other
products in the @@url{https://unidoc.io/,@code{UniDoc}} ecosystem in order to
provide charting capabilities.")
    (license license:expat)))

(define-public go-github-com-unidoc-unitype
  (package
    (name "go-github-com-unidoc-unitype")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/unitype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jxm6vi5711rsqgpvm5sswydfr9mylqy3dzam5z9jnssbsj74p6g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/unitype"))
    (propagated-inputs (list go-golang-org-x-text
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus))
    (home-page "https://github.com/unidoc/unitype")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-unidoc-unipdf-v3
  (package
    (name "go-github-com-unidoc-unipdf-v3")
    (version "3.58.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unidoc/unipdf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dyqk9hmjhqmij3d26zq8b7f6nv04cp1frz7mz4rs8spy0gzgxwb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unidoc/unipdf/v3"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Source-only package.
          (delete 'build))
      ))
    (propagated-inputs (list go-golang-org-x-xerrors
                             go-golang-org-x-text
                             go-golang-org-x-net
                             go-golang-org-x-image
                             go-golang-org-x-crypto
                             go-github-com-unidoc-unitype
                             go-github-com-unidoc-unichart
                             go-github-com-unidoc-timestamp
                             go-github-com-unidoc-pkcs7
                             go-github-com-unidoc-garabic
                             go-github-com-unidoc-freetype
                             go-github-com-trimmer-io-go-xmp
                             go-github-com-stretchr-testify
                             go-github-com-gorilla-i18n
                             go-github-com-gabriel-vasile-mimetype
                             go-github-com-boombuler-barcode
                             go-github-com-adrg-sysfont))
    (home-page "https://github.com/unidoc/unipdf")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-juruen-rmapi
  (package
    (name "go-github-com-juruen-rmapi")
    (version "0.0.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juruen/rmapi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mpnjy6s6g99z76b59imcsdfl0vsqz1b08650wwvrjsyvdvh577f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.19
      ;; #:install-source? #f
      #:import-path "github.com/juruen/rmapi"))
    (propagated-inputs (list go-github-com-google-shlex
                         go-gopkg-in-yaml-v2
                         go-golang-org-x-sync
                         go-github-com-unidoc-unipdf-v3
                         go-github-com-stretchr-testify
                         go-github-com-pkg-errors
                         go-github-com-nfnt-resize
                         go-github-com-google-uuid
                         go-github-com-golang-jwt-jwt
                         go-github-com-abiosoft-ishell))
    (home-page "https://github.com/juruen/rmapi")
    (synopsis "rMAPI")
    (description
     "@code{rMAPI} is a Go app that allows you to access the @code{ReMarkable} Cloud
API programmatically.")
    (license license:agpl3)))
