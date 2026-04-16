(define-module (trisk services asus)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (trisk packages asus)
  #:export (supergfxd-service-type
            supergfxd-configuration))


(define-configuration/no-serialization supergfxd-configuration
  (supergfxctl
   (file-like supergfxctl)
   "Package to use.")
  (log-file
   (string "/var/log/supergfxd.log")
   "Path to log-file"))

(define (supergfxd-package config)
  (list (supergfxd-configuration-supergfxctl config)))

(define supergfxd-shepherd-service
  (match-record-lambda <supergfxd-configuration>
      (supergfxctl log-file)
    (list (shepherd-service
           (provision '(supergfxd))
           (requirement '(user-processes udev dbus-system))
           (start #~(make-forkexec-constructor
                     (list #$(file-append supergfxctl "/bin/supergfxd"))
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

(define supergfxd-service-type
  (service-type
    (name 'supergfxd)
    (extensions (list
                 (service-extension shepherd-root-service-type
                                    supergfxd-shepherd-service)
                 (service-extension dbus-root-service-type
                                    supergfxd-package)
                 (service-extension udev-service-type
                                    supergfxd-package)
                 (service-extension profile-service-type
                                    supergfxd-package)))
    (default-value (supergfxd-configuration))
    (description "Run the Power Profiles Daemon")))
