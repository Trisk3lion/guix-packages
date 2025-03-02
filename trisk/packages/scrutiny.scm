(define-module (trisk packages scrutiny)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
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
  #:use-module (trisk packages golang-xyz))

(define-public scrutiny-collector
  (package
    (name "scrutiny-collector")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AnalogJ/scrutiny")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n5hqmwb29mdqphzn55bpq5rgcdnmk3mvs23z981c4h8vfnkk1as"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/analogj/scrutiny/collector/cmd/collector-metrics"
      #:unpack-path "github.com/analogj/scrutiny"))
    (propagated-inputs (list go-gorm-io-gorm
                             go-golang-org-x-sync
                             go-github-com-urfave-cli-v2
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-sirupsen-logrus
                             go-github-com-samber-lo
                             go-github-com-mitchellh-mapstructure
                             go-github-com-jaypipes-ghw
                             go-github-com-influxdata-influxdb-client-go-v2
                             go-github-com-golang-mock
                             go-github-com-go-gormigrate-gormigrate-v2
                             go-github-com-glebarez-sqlite
                             go-github-com-gin-gonic-gin
                             go-github-com-fatih-color
                             go-github-com-containrrr-shoutrrr
                             go-github-com-analogj-go-util))
    (inputs (list smartmontools))
    (home-page "https://github.com/analogj/scrutiny")
    (synopsis "scrutiny")
    (description "@code{WebUI} for smartd S.M.A.R.T monitoring.")
    (license license:expat)))

(define-public scrutiny-web
  (package
    (name "scrutiny-web")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AnalogJ/scrutiny")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n5hqmwb29mdqphzn55bpq5rgcdnmk3mvs23z981c4h8vfnkk1as"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/analogj/scrutiny/webapp/backend/cmd/scrutiny"
      #:unpack-path "github.com/analogj/scrutiny"))
    (propagated-inputs (list go-gorm-io-gorm
                             go-golang-org-x-sync
                             go-github-com-urfave-cli-v2
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-sirupsen-logrus
                             go-github-com-samber-lo
                             go-github-com-mitchellh-mapstructure
                             go-github-com-jaypipes-ghw
                             go-github-com-influxdata-influxdb-client-go-v2
                             go-github-com-golang-mock
                             go-github-com-go-gormigrate-gormigrate-v2
                             go-github-com-glebarez-sqlite
                             go-github-com-gin-gonic-gin
                             go-github-com-fatih-color
                             go-github-com-containrrr-shoutrrr
                             go-github-com-analogj-go-util))
    (home-page "https://github.com/analogj/scrutiny")
    (synopsis "scrutiny")
    (description "@code{WebUI} for smartd S.M.A.R.T monitoring.")
    (license license:expat)))
