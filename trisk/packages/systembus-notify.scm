(define-module (trisk packages systembus-notify)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages check) ;; For cppcheck
  #:use-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:))


(define-public systembus-notify
  (let ((commit "138c8f5ba08e0639d592dabbe05e89b57c6035c1")
        (revision "0"))
  (package
    (name "systembus-notify")
    (version (git-version "1.1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rfjakob/systembus-notify/")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jhqddlwb39q22gmg0n1sm13z71i2cb1391myf49v9bhxsj82rrn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'install
                     (lambda _
                       (install-file "systembus-notify" (string-append #$output "/bin"))
                       (install-file "systembus-notify.desktop" (string-append #$output "/share/applications")))))))
    (native-inputs (list pkg-config
                         ;; cppcheck
                         ))
    (inputs (list elogind))
    (synopsis "systembus-notify - system bus notification daemon ")
    (description "Tiny daemon that listens for net.nuetzlich.SystemNotifications.Notify signals on the D-Bus system bus and shows them as desktop notifications using the user bus.")
    (home-page "https://github.com/rfjakob/systembus-notify")
    (license license:expat))))
