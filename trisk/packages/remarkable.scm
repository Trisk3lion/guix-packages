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
  #:use-module (trisk packages golang-xyz)
  #:use-module (gnu packages syncthing)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system go))

;; d07b7360a2ef5914c72194395850731462b7b520
;; (define-public remy
;;   (let ((version "0.0.0")
;;         (revision "0")
;;         (commit "df2c1aec2efe8ba8f1f2ae098478eb8f580188dd"))
;;    (package
;;     (name "remy")
;;     (version (git-version version revision commit))
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/bordaigorl/remy")
;;                     (commit commit)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "1x2n58q8gpfkmwxqk0zrv069diamigcs35fn99lfn98412hqvdwi"))))
;;     (build-system python-build-system)
;;     (arguments
;;      (list #:tests? #f))
;;     (propagated-inputs (list python-arrow
;;                              python-paramiko
;;                              python-pypdf
;;                              python-pyqt
;;                              python-requests
;;                              python-sip))
;;     (home-page "https://github.com/bordaigorl/remy")
;;     (synopsis "Online&offline manager for the reMarkable tablet")
;;     (description "The goal of Remy is to allow simple interaction with the
;; reMarkable tablet over ssh, without needing the cloud service, nor the USB Web
;; UI.")
;;     (license license:gpl3+))))

(define-public rmapi
  (package
    (name "rmapi")
    (version "0.0.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ddvk/rmapi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yspx79jvmdiz4aiphhl8giq4m0501bl9dnnifybc57y5v99ds7x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
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
                         go-github-com-abiosoft-ishell
                         go-github-com-ogier-pflag))
    (home-page "https://github.com/juruen/rmapi")
    (synopsis "rMAPI")
    (description
     "@code{rMAPI} is a Go app that allows you to access the @code{ReMarkable} Cloud
API programmatically.")
    (license license:agpl3)))
