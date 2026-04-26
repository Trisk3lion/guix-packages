(define-module (shika packages steam)
  #:use-module (nongnu packages game-client)
  #:use-module (trisk packages gamescope)
  #:use-module (nonguix utils)
  #:use-module (gnu packages gl)
  #:use-module (nonguix multiarch-container))

(define-public (steam-gamescope-container-for driver)
  (let ((steam-container (steam-container-for driver)))
    (nonguix-container
     (name (ngc-name steam-container))
     (wrap-package (ngc-wrap-package steam-container))
     (run (ngc-run steam-container))
     (packages
      `(("gamescope" ,gamescope)
        ,@(ngc-packages steam-container)))
     (preserved-env (ngc-preserved-env steam-container))
     (link-files (ngc-link-files steam-container))
     (description (string-append (ngc-description steam-container)
                                 "  With gamescope included in environment.")))))

(define-public steam-gamescope-for
  (compose nonguix-container->package steam-gamescope-container-for))

(define-public steam-gamescope
  (package-with-alias "steam-gamescope" (steam-gamescope-for mesa)))

;;; steam-gamescope-nvidia is removed because of new nonguix approach of using it.
;;; to use steam-gamescope on nvidia system add (steam-gamescope-for nvda-580) to packages.
;;; replace nvda-580 with version you want.
