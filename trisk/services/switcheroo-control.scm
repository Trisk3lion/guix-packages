(define-module (trisk services switcheroo-control)
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
  #:use-module (trisk packages switcheroo-control)
  #:export (switcheroo-service-type
            switcheroo-configuration))


(define-configuration/no-serialization switcheroo-configuration
  (switcheroo-control
   (file-like switcheroo-control)
   "Package to use.")
  (log-file
   (string "/var/log/switcheroo.log")
   "Path to log-file"))

(define (switcheroo-package config)
  (list (switcheroo-configuration-switcheroo-control config)))

(define switcheroo-shepherd-service
  (match-record-lambda <switcheroo-configuration>
      (switcheroo-control log-file)
    (list (shepherd-service
            (provision '(switcheroo-control))
            (requirement '(user-processes udev dbus-system))
            (start #~(make-forkexec-constructor
                      (list #$(file-append switcheroo-control "/libexec/switcheroo-control")
                            "--verbose")
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor))))))

(define switcheroo-service-type
  (service-type
    (name 'switcheroo-control)
    (extensions (list
                 (service-extension shepherd-root-service-type
                                    switcheroo-shepherd-service)
                 (service-extension dbus-root-service-type
                                    switcheroo-package)
                 (service-extension udev-service-type
                                    switcheroo-package)
                 (service-extension profile-service-type
                                    switcheroo-package)))
    (default-value (switcheroo-configuration))
    (description "Run the switcheroo-control daemon")))
