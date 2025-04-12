(define-module (trisk packages fwupd)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((nongnu packages firmware) :prefix nonguix:)
  )

(define-public fwupd-nonfree
  (package
    (inherit nonguix:fwupd-nonfree)
    (source
     (origin
       (inherit (package-source nonguix:fwupd-nonfree))
       (patches
        (parameterize
            ((%patch-path
              (map (lambda (directory)
                     (string-append directory "/trisk/packages/patches"))
                   %load-path)))
          (search-patches
           ;; meson configure option for polkit actions dir
           "fwupd-meson-configure.patch")))))
    (arguments
     (substitute-keyword-arguments (package-arguments nonguix:fwupd-nonfree)
       ((#:configure-flags flags #~'())
        #~(cons "-DPOLKIT_ACTIONDIR=/etc/polkit-1/actions" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; upstream to nonguix
            (delete 'ensure-all-remotes-are-disabled)
            ;; end upstream to nonguix
            ))))))
