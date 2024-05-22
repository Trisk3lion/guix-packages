(define-module (trisk packages remarkable)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject))

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
