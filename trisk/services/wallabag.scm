(define-module (trisk services wallabag)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services docker)
  #:use-module (gnu services containers)
  #:use-module (gnu system shadow)
  #:export (wallabag-configuration
            oci-wallabag-service-type))

(define-maybe string)

(define-configuration wallabag-configuration
  (tag
   (string "2.6.14")
   "Docker version tag to use.")
  (cache-directory
   (string "/var/cache/wallabag")
   "Path to cache directory.")
  (data-directory
   (string "/var/lib/wallabag")
   "Path to configuration directory.")
  (email
   (string "")
   "Email address to the admin of Wallabag.")
  (domain-name
   (string "http://localhost")
   "Domain name.")
  (server-name
   (string "wallabag")
   "Server name.")
  (expose-port
   (number 8015)
   "Port to expose wallabag server on.")
  (log-file
   (string "/var/log/wallabag.log")
   "Path to log file.")
  (requirement
   (list-of-symbols '())
   "List of symbols for other shepherd services which we require.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

;; TODO: database-configuration
;; (define-configuration/no-serialization wallabag-sqlite-configuration
;;   (driver
;;    (string "pdo_sqlite")
;;    "Name of the SQLITE driver.")
;;   (host
;;    (string "127.0.0.1")
;;    "Host address of database connection."))

;; (define-configuration/no-serialization wallabag-postgres-configuration
;;   (driver
;;    (string "pdo_sqlite")
;;    "Name of the SQLITE driver.")
;;   (host
;;    (string "127.0.0.1")
;;    "Host address of database connection."))

(define %wallabag-accounts
  (list (user-account
          (name "wallabag")
          (group "docker")
          (system? #t)
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define wallabag-log-rotations
  (match-record-lambda <wallabag-configuration>
      (log-file)
    (list log-file)))

(define wallabag-activation
  (match-record-lambda <wallabag-configuration>
      (cache-directory data-directory)
    #~(let ((user (getpwnam "wallabag")))
        (for-each
         (lambda (directory)
           (unless (file-exists? directory)
             (mkdir-p directory)
             (chown directory (passwd:uid user) (passwd:gid user))))
         '#$(list cache-directory data-directory)))))

(define wallabag-oci-containers
  (match-record-lambda <wallabag-configuration>
      (tag cache-directory data-directory email domain-name
           server-name expose-port log-file requirement extra-options)
    (oci-extension
     (containers
      (list
       (oci-container-configuration
         (provision "wallabag-oci")
         (user "wallabag")
         (group "docker")
         (image (string-append "wallabag/wallabag:" tag))
         (environment
          `("SYMFONY__ENV__DATABASE_DRIVER=pdo_sqlite"
            "SYMFONY__ENV__DATABASE_HOST=127.0.0.1"
            "SYMFONY__ENV__MAILER_HOST=127.0.0.1"
            "SYMFONY__ENV__TWOFACTOR_AUTH=false"
            "SYMFONY__ENV__FOSUSER_REGISTRATION=true"
            ,(string-append "SYMFONY__ENV__FROM_EMAIL=" email)
            ,(string-append "SYMFONY__ENV__DOMAIN_NAME=" domain-name)
            ,(string-append "SYMFONY__ENV__SERVER_NAME=\"" server-name "\"")))
         (ports
          `((,(number->string expose-port) . "80")))
         (log-file log-file)
         (respawn? #t)
         (requirement requirement)
         (network "host")
         (volumes
          `((,cache-directory . "/var/www/wallabag/web/assets/images")
            (,data-directory . "/var/www/wallabag/data")))
         (extra-arguments extra-options)))))))

(define oci-wallabag-service-type
  (service-type
   (name 'wallabag-oci)
   (extensions
    (list (service-extension account-service-type
                             (const %wallabag-accounts))
          (service-extension activation-service-type
                             wallabag-activation)
          (service-extension log-rotation-service-type
                             wallabag-log-rotations)
          (service-extension oci-service-type
                             wallabag-oci-containers)))
   (default-value (wallabag-configuration))
   (description "Run Wallab, a read-it-later application.")))
