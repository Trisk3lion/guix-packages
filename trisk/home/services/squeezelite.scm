(define-module (trisk home services squeezelite)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (trisk services squeezelite)
  #:use-module ((gnu system shadow) #:select (account-service-type))
  ;; For the 'home-shepherd-service-type' mapping.
  #:use-module (gnu home services shepherd)
  #:export (home-squeezelite-service-type)
  #:re-export (squeezelite-configuration
	       squeezelite-configuration?))


(define home-squeezelite-service-type
  (service-type
    (inherit (system->home-service-type
              (remove-service-extensions squeezelite-service-type
                                         (list account-service-type))))
    (default-value (for-home (squeezelite-configuration)))))
