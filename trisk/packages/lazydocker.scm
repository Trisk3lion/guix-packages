(define-module (trisk packages lazydocker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages prometheus)
  #:use-module (trisk packages golang-xyz)
  )

(define-public go-github-com-jesseduffield-lazydocker
  (package
    (name "go-github-com-jesseduffield-lazydocker")
    (version "0.24.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jesseduffield/lazydocker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lswyxkakf97rcmvwjjj9zvgfgh5fz83z5pmiyhxy6k6pivc6n3i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/jesseduffield/lazydocker"))
    (propagated-inputs (list go-github-com-goccy-go-yaml
                             go-golang-org-x-xerrors
                             go-github-com-stretchr-testify
                             go-github-com-spkg-bom
                             go-github-com-sirupsen-logrus
                             go-github-com-sasha-s-go-deadlock
                             go-github-com-samber-lo
                             go-github-com-pmezard-go-difflib
                             go-github-com-mgutz-str
                             go-github-com-mcuadros-go-lookup
                             go-github-com-mattn-go-runewidth
                             go-github-com-jesseduffield-yaml
                             go-github-com-jesseduffield-lazycore
                             go-github-com-jesseduffield-kill
                             go-github-com-jesseduffield-gocui
                             go-github-com-jesseduffield-asciigraph
                             go-github-com-integrii-flaggy
                             go-github-com-imdario-mergo
                             go-github-com-gookit-color
                             go-github-com-go-errors-errors
                             go-github-com-fatih-color
                             go-github-com-docker-docker
                             go-github-com-docker-cli
                             go-github-com-cloudfoundry-jibber-jabber
                             go-github-com-boz-go-throttle
                             go-github-com-openpeedeep-xdg))
    (home-page "https://github.com/jesseduffield/lazydocker")
    (synopsis "Sponsors")
    (description
     "This package provides a simple terminal UI for both docker and docker-compose,
written in Go with the @@url{https://github.com/jroimartin/gocui,gocui} library.")
    (license license:expat)))
