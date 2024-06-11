(define-module (trisk services autofs)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services admin)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (gnu packages file-systems)
  #:use-module (ice-9 match)
  #:export (autofs-configuration
            autofs-configuration?
            autofs-service-type

            autofs-mount-configuration
            autofs-mount-configuration?

            ;; XXX: Don't need to export autofs-configuration-file
            autofs-configuration-file))

;;; Commentary:
;;;
;;; This module provides a service definition for the SERVICE_NAME
;;; SERVICE_DESCRTIPTION
;;;
;;; Code:

(define %autofs-log-file
  "/var/log/autofs.log")

;; (define (list-of-autofs-mount-configurations? lst)
;;   (every autofs-mount-configuration? lst))

;; (define-maybe list)

;; (define-configuration autofs-configuration
;;   (autofs
;;    (file-like autofs)
;;    "The autofs package to use")
;;   (mounts
;;    (list-of-autofs-mount-configurations? '())))

;; (define-configuration autofs-mount-configuration
;;   (target
;;    (string)
;;    "Mount point target.")
;;   (source
;;    (string)
;;    "Mount point source.")
;;   (options
;;    maybe-list
;;    "List of string representing mount options."))

(define-record-type* <autofs-configuration>
  autofs-configuration make-autofs-configuration
  autofs-configuration?
  (autofs autofs-configuration-autofs                   ; file-like
          (default autofs))
  (pid-file autofs-configuration-pid-file               ; string
            (default "/var/run/autofs"))
  (config-file autofs-configuration-config-file         ; sring | file-like object
               (default (plain-file "empty" "")))
  (mounts autofs-configuration-mounts ;list of <autofs-mount-configuration>
          (default '()))
  (options autofs-configuration-options
           (default "--timeout=5")))

(define-record-type* <autofs-mount-configuration>
  autofs-mount-configuration make-autofs-mount-configuration
  autofs-mount-configuration?
  (target autofs-mount-configuration-target   ; string
          (default #f))
  (source autofs-mount-configuration-source   ; string
          (default #f))
  (options autofs-mount-configuration-options ; list of strings
          (default '())))

(define (autofs-configuration-file config)
  (define autofs-mounts-configuration-file
    (plain-file "autofs-mounts.conf"
                (call-with-output-string
                  (lambda (port)
                    (match-record config <autofs-configuration>
                                  (autofs pid-file config-file mounts)
                      (for-each (lambda (mount)
                                  (match-record mount <autofs-mount-configuration>
                                                (target source options)
                                    (display (string-join (list target (string-join options ",") source))
                                             port)
                                    (newline port)))
                                mounts))))))
    (mixed-text-file "autofs.conf"
                     "/- " autofs-mounts-configuration-file " --timeout=10"
                                        ;; (autofs-configuration-options config)
                                        ))

(define (autofs-activation config)
  "Return the activation gexp for CONFIG."
  (let ((targets (map autofs-mount-configuration-target
                      (autofs-configuration-mounts config))))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/etc/autofs")
        (for-each mkdir-p '#$targets))))

(define (autofs-shepherd-service config)
  ;; Return a <shepherd-service> running autofs.
  (match-record config <autofs-configuration>
    (autofs pid-file config-file mounts)
    (list (shepherd-service
           (provision '(autofs))
           (documentation "Run autofs daemon.")
           (requirement '(user-processes)) ;; loopback? networking also?
           (start #~(make-forkexec-constructor
                     (list #$(file-append autofs "/sbin/automount")
                           "-f" "-p" #$pid-file
                           #$(autofs-configuration-file config))
                     #:pid-file #$pid-file
                     #:log-file #$%autofs-log-file))
           (stop #~(make-kill-destructor))
           (actions (list (shepherd-configuration-action config-file)))))))

(define %autofs-log-rotations
  (list (log-rotation
         (files (list %autofs-log-file)))))

(define autofs-service-type
  (service-type
   (name 'autofs)
   (extensions
    (list (service-extension activation-service-type
                             autofs-activation)
          (service-extension shepherd-root-service-type
                             autofs-shepherd-service)
          (service-extension rottlog-service-type
                             (const %autofs-log-rotations))))
   (default-value (autofs-configuration))
   (description
    "Run the autofs.")))
