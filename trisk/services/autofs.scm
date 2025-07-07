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
  #:use-module (srfi srfi-1)
  #:export (autofs-configuration
            autofs-configuration?
            autofs-service-type
            autofs-mount-configuration
            autofs-mount-configuration?
            autofs-configuration-file))

;;; Commentary:
;;;
;;; This module provides a service definition for the SERVICE_NAME
;;; SERVICE_DESCRTIPTION
;;;
;;; Code:

(define %autofs-log-file
  "/var/log/autofs.log")

;; (define %dummy-config
;;   (plain-file "empty" ""))

(define-maybe/no-serialization list)

(define-configuration autofs-mount-configuration
  (target
   string
   "Mount point target.")
  (source
   string
   "Mount point source.")
  (options
   maybe-list
   "List of string representing mount options.")
  (no-serialization))

(define (list-of-autofs-mount-configurations? lst)
  (every autofs-mount-configuration? lst))

(define-configuration autofs-configuration
  (autofs
   (file-like autofs)
   "The autofs package to use.")
  (pid-file
   (string "/var/run/autofs")
   "Location of the PID file.")
  ;; (config-file
  ;;  (file-like %dummy-config)
  ;;  "Empty dummy config file.")
  (mounts
   (list-of-autofs-mount-configurations '())
   "List of mount configuration.")
  (caching-timeout
   (integer 60)
   "The default timeout for caching failed key lookups. The default is 60 seconds.")
  (unmount-timeout
   (integer 600)
   "The Global minimum timeout, in seconds, until directories are unmounted. The default is 10 minutes. Setting the timeout to zero disables umounts completely.")
  (no-serialization))

(define (autofs-configuration-file config)

  (define autofs-mounts-configuration-file
    (plain-file "autofs.mounts.conf"
                (call-with-output-string
                  (lambda (port)
                    (match-record config <autofs-configuration> (mounts)
                                  (for-each (lambda (mount)
                                              (match-record mount <autofs-mount-configuration>
                                                (target source options)
                                                (let ((opts (if (maybe-value-set? options)
                                                                (string-join options ",") "")))
                                                  (format port "~a ~a ~a\n" target opts source))))
                                            mounts))))))
  (match-record config <autofs-configuration> (unmount-timeout)
    (mixed-text-file "autofs.master"
                     "/- " autofs-mounts-configuration-file
                     (format #f " --timeout=~a" unmount-timeout) "\n")))

(define (autofs-activation config)
  "Return the activation gexp for CONFIG."
  (let ((targets (map autofs-mount-configuration-target
                      (autofs-configuration-mounts config))))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/etc/autofs")
        ;; (call-with-output-file "/etc/autofs.conf"
;;             (lambda (port)
;;               (display (string-append "
;; [ autofs ]
;; master_map_name = " #$(autofs-configuration-file config) "
;; timeout = 300
;; ") port)))
        (for-each mkdir-p '#$targets))))

(define (autofs-shepherd-service config)
  ;; Return a <shepherd-service> running autofs.
  (match-record config <autofs-configuration>
    (autofs pid-file mounts caching-timeout)
    (let ((config-file (autofs-configuration-file config))
          (cache (number->string caching-timeout)))
      (list (shepherd-service
             (provision '(autofs))
             (documentation "Run autofs daemon.")
             (requirement '(user-processes networking)) ;; loopback? networking also?
             (start #~(make-forkexec-constructor
                       (list #$(file-append autofs "/sbin/automount")
                             "-f" "-p" #$pid-file "-n" #$cache
                             #$config-file)
                       #:pid-file #$pid-file
                       #:log-file #$%autofs-log-file))
             (stop #~(make-kill-destructor))
             (actions (list (shepherd-configuration-action config-file))))))))

(define %autofs-log-rotations
  (list %autofs-log-file))

(define autofs-service-type
  (service-type
   (name 'autofs)
   (extensions
    (list (service-extension activation-service-type
                             autofs-activation)
          (service-extension shepherd-root-service-type
                             autofs-shepherd-service)
          (service-extension log-rotation-service-type
                             (const %autofs-log-rotations))))
   (default-value (autofs-configuration))
   (description
    "Run the autofs.")))
