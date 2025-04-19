(define-module (trisk services keyd)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages linux)
  #:export (keyd-service-type
            keyd-configuration
            keyd-configuration?))

(define-configuration/no-serialization keyd-configuration
  (keyd
   (file-like keyd)
   "The keyd package.")

  (user
   (string "keyd")
   "The name of the user under which Keyd will be executed.")

  (group
   (string "keyd")
   "The name of the group under which Keyd will be executed.")

  (config
   (string "[ids]

*

[main]")
   "Keyd config as a string."))

(define (keyd-configuration-file config)
  `(("keyd/config.conf" . ,(plain-file "config.conf"
                                       (string-append (keyd-configuration-config config)
                                                    "\n")))))

(define (keyd-shepherd-service config)
  "Return a <shepherd-service> for Forgejo with config."
  (match-record config <keyd-configuration>
    (keyd user group)
    (list (shepherd-service
           (documentation "Run the Keyd daemon.")
           (requirement '(user-processes))
           (provision '(keyd))
           ;; (actions (list (shepherd-configuration-action keyd-reload)))
           (start #~(make-forkexec-constructor
                     (list #$(file-append keyd "/bin/keyd"))
                     ;; #:user #$user
                     ;; #:group #$group
                     ))
           (stop  #~(make-kill-destructor))))))

(define %keyd-accounts
  (list (user-group (name "keyd")
                    (system? #t))
        (user-account
         (name "keyd")
         (group "keyd")
         (supplementary-groups '("input"))
         (system? #t)
         (comment "Keyd User")
         (home-directory "/var/empty"))))

(define (keyd-activation config)
  (with-imported-modules '((guix build utils))
  #~(begin
      (use-modules (guix build utils))
      ;; (mkdir-p "/etc/keyd")
      (system* #$(file-append (keyd-configuration-keyd config)
                              "/bin/keyd" "reload"))

      ;; (let ((user (getpwnam "keyd")))
      ;;   (chown "/srv/keyd"
      ;;          (passwd:uid user)
      ;;          (passwd:gid user)))
      )))

(define keyd-service-type
  (service-type (name 'keyd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          keyd-shepherd-service)
                       (service-extension etc-service-type
                                          keyd-configuration-file)
                       ;; (service-extension account-service-type
                       ;;                    (const %keyd-accounts))
                       (service-extension activation-service-type
                                          keyd-activation)))
                (default-value (keyd-configuration))
                (description "Run Keyd keyboard daemon.")))
