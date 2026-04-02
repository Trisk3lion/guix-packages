(define-module (trisk services asus)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (trisk packages asus)
  #:export (powergfxd-service-type
            powergfxd-configuration))


(define-configuration/no-serialization powergfxd-configuration
  (powercfgd
   (file-like supergfxctl)
   "Package to use."))

(define (powergfxd-package config)
  (list (powergfxd-configuration-powercfgd config)))

(define powergfxd-shepherd-service
  (match-record-lambda <powergfxd-configuration>
      (powergfxd)
  (list (shepherd-service
          (provision '(powergfxd))
          (requirement '(user-processes dbus))
          (start #~(make-forkexec-constructor
                    (list #$(file-append powergfxd "/bin/powergfxd"))))
          (stop #~(make-kill-destructor))))))

(define powergfxd-service-type
  (service-type
    (name 'power-profiles-daemon)
    (extensions (list
                 (service-extension shepherd-root-service-type
                                    powergfxd-shepherd-service)
                 (service-extension dbus-root-service-type
                                    powergfxd-package)
                 (service-extension udev-service-type
                                    powergfxd-package)
                 ;; (service-extension polkit-service-type
                 ;;                    config->package)
                 (service-extension profile-service-type
                                    powergfxd-package)
                 ;; (service-extension activation-service-type
                 ;;                    (const %power-profiles-daemon-activation))
                 ))
    (default-value (powergfxd-configuration))
    (description "Run the Power Profiles Daemon")))
