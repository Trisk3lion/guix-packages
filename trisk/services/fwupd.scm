(define-module (trisk services fwupd)
  #:use-module (gnu packages firmware)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (fwupd-configuration
            fwupd-configuration?
            fwupd-configuration-fields
            fwupd-configuration-fwupd
            fwupd-service-type))

(define-configuration/no-serialization fwupd-configuration
  (fwupd
   (file-like fwupd)
   "The fwupd package that will be installed in the system profile."))

(define (fwupd-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/var/cache/fwupd")
        (mkdir-p "/var/lib/fwupd/local.d")
        (mkdir-p "/var/lib/fwupd/"))))

(define (fwupd-shepherd-services config)
  (let ((fwupd (fwupd-configuration-fwupd config)))
    (list
     (shepherd-service (provision '(fwupd))
                       (requirement '(user-processes))
                       (documentation "Run the fwupd daemon.")
                       (start #~(make-forkexec-constructor
                                 (list #$(file-append fwupd "/libexec/fwupd/fwupd"))))
                       (stop #~(make-kill-destructor))))))

(define fwupd-service-type
  (service-type (name 'fwupd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          fwupd-shepherd-services)
                       (service-extension profile-service-type
                                          (compose list fwupd-configuration-fwupd))
                       (service-extension dbus-root-service-type
                                          (compose list fwupd-configuration-fwupd))
                       (service-extension polkit-service-type
                                          (compose list fwupd-configuration-fwupd))
                       (service-extension activation-service-type
                                          fwupd-activation)))
                (default-value (fwupd-configuration))
                (description
                 "This service configures fwupd on the Guix System.")))
