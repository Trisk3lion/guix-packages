(define-module (trisk services clipcascade)
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
  #:use-module (trisk packages clipcascade)
  #:export (clipcascade-service-type
            clipcascade-configuration
            clipcascade-configuration?))

(define-configuration/no-serialization clipcascade-configuration
  (clipcascade
   (file-like clipcascade)
   "The clipcascade package.")

  (host
   (string "localhost")
   "The host name of the server.")

  (port 
   (string "8080")
   "The host name of the server.")

  (origins
   (string "*")
   "Allowed origins.")

  (enable-p2p?
   (boolean #f)
   "Enable P2P connections.")

  (user
   (string "clipcascade")
   "The name of the user under which Forgejo will be executed.")

  (group
   (string "clipcascade")
   "The name of the group under which Forgejo will be executed."))

(define (clipcascade-shepherd-service config)
  "Return a <shepherd-service> for Forgejo with config."
  (match-record config <clipcascade-configuration>
    (clipcascade port origins user group)
    (list (shepherd-service
           (documentation "Run the Clipcascade server.")
           (requirement '(networking user-processes))
           (provision '(clipcascade))
           (actions (list (shepherd-configuration-action clipcascade-cfg)))
           (start #~(make-forkexec-constructor
                     (list #$(file-append clipcascade "/bin/clipcascade"))
                     #:user #$user
                     #:group #$group
                     #:environment-variables (append (default-environment-variables)
                                                     (list (string-append "CC_PORT=" #$port)
                                                           (string-append "CC_ALLOWED_ORIGINS=" #$origins)
                                                           (string-append "CC_P2P_ENABLED=" (if #$enable-p2p? "false" "true"))))))
           (stop  #~(make-kill-destructor))))))

(define %clipcascade-accounts
  (list (user-group (name "clipcascade")
                    (system? #t))
        (user-account
         (name "clipcascade")
         (group "clipcascade")
         (system? #t)
         (comment "Clipcascade User")
         (home-directory "/var/empty"))))

(define %clipcascade-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/srv/clipcascade")
      (let ((user (getpwnam "clipcascade")))
        (chown "/srv/clipcascade"
               (passwd:uid user)
               (passwd:gid user)))))

(define clipcascade-service-type
  (service-type (name 'clipcascade)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          clipcascade-shepherd-service)
                       (service-extension account-service-type
                                          (const %clipcascade-accounts))
                       (service-extension activation-service-type
                                          (const %clipcascade-activation))))
                (default-value (clipcascade-configuration))
                (description "Run Clipcascade server.")))
