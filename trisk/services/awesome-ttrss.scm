(define-module (trisk services awesome-ttrss)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages docker)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:export (awesome-ttrss-configuration
            awesome-ttrss-configuration?
            awesome-ttrss-configuration-fields
            awesome-ttrss-configuration-uid
            awesome-ttrss-configuration-gid
            awesome-ttrss-configuration-ttrss-image
            awesome-ttrss-configuration-postgres-image
            awesome-ttrss-configuration-db-name
            awesome-ttrss-configuration-db-user
            awesome-ttrss-configuration-db-pass
            awesome-ttrss-configuration-port
            awesome-ttrss-configuration-postgres-datadir
            awesome-ttrss-configuration-ttrss-plugins
            %awesome-ttrss-accounts
            %awesome-ttrss-activation
            awesome-ttrss-configuration->oci-container-configuration
            oci-awesome-ttrss-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define awesome-ttrss-tag
  "latest")
(define mercury-tag
  "latest")
(define postgres-tag
  "12-alpine")

(define awesome-ttrss-image
  (string-append "wangqiru/ttrss:" awesome-ttrss-tag))
(define %mercury-image
  (string-append "wangqiru/mercury-parser-api:" mercury-tag))
(define postgres-image
  (string-append "postgres:" postgres-tag))

(define-maybe/no-serialization file-like)

(define-configuration/no-serialization awesome-ttrss-configuration
  (uid
   (positive 34595)
   "The uid assigned to the awsome-ttrss service account.")
  (gid
   (positive 98715)
   "The gid assigned to the awsome-ttrss service account.")
  (ttrss-image
   (string awesome-ttrss-image)
   "The TTRSS image to use for the OCI backed Shepherd service.")
  (postgres-image
   (string awesome-ttrss-image)
   "The postgres image to use for the OCI backed Shepherd service.")
  (port
   (string "3000")
   "The port where awesome-ttrss will be exposed.")
  (postgres-datadir
   (string "/var/lib/postgresql/data"))
  (db-name
   (string "ttrss")
   "Postgres database name.")
  (db-user
   (string "postgres")
   "Postgres database user.")
  (db-pass
   (string "password")
   "Postgres database password.")
  (ttrss-plugins
   (string "auth_internal,fever,api_feedreader")
   "Comma separated string of plugins to add to TTRSS."))

(define (%awesome-ttrss-accounts config)
  (list (user-group
         (system? #t)
         (name "ttrss")
         (id (awesome-ttrss-configuration-gid config)))
        (user-account
         (name "ttrss")
         (uid (awesome-ttrss-configuration-uid config))
         (comment "Awesome-TTRSS's Service Account")
         (group "ttrss")
         (supplementary-groups '("tty"))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))


(define (%awesome-ttrss-activation config)
  "Return an activation gexp for awsome-ttrss."
  (let* ((datadir (awesome-ttrss-configuration-postgres-datadir config))
         ;; (gid (awesome-ttrss-configuration-gid config))
         ;; (uid (awesome-ttrss-configuration-uid config))
         (docker (file-append docker-cli "/bin/docker")))
    #~(begin
        (use-modules (guix build utils))
        ;; (let ((user (getpwnam "conduit"))
        ;;       (uid (passwd:uid user))
        ;;       (gid (passwd:gid user)))
        (let ((datadir #$datadir)
              (gid #$gid)
              (uid #$uid))
          ;; Setup datadir
          (mkdir-p datadir)
          (chown datadir uid gid)
          (chmod datadir #o770)
          ;; Create docker network
          (invoke #$docker "network" "create" "ttrss")))))

(define (awesome-ttrss-configuration->oci-container-configuration config)
  (let ((datadir (awesome-ttrss-configuration-postgres-datadir config))
        (uid (awesome-ttrss-configuration-uid config))
        (gid (awesome-ttrss-configuration-gid config))
        (ttrss-image (awesome-ttrss-configuration-image config))
        (postgres-image (awesome-ttrss-configuration-image config))
        (db-name (awesome-ttrss-configuration-db-name config))
        (db-user (awesome-ttrss-configuration-db-user config))
        (db-pass (awesome-ttrss-configuration-db-pass config))
        (db-port (awesome-ttrss-configuration-port config))
        (plugins (awesome-ttrss-configuration-ttrss-plugins config)))
    (list (oci-container-configuration
           (image ttrss-image)
           ;; (requirement '(database.postgres))
           (environment
            `(("USER_UID" . ,(number->string uid))
              ("USER_GID" . ,(number->string gid))
              ("SELF_URL_PATH" . ,url-path)
              ("DB_HOST" . "database.postgres")
              ("DB_USER" . ,db-user)
              ("DB_PASS" . ,db-pass)
              ("ENABLE_PLUGINS" . ,plugins)
              ("FEED_LOG_QUIT" . "true")))
           (ports
            `((,port . "80")))
           (volumes
            '(("/etc/timezone" . "/etc/timezone:ro")
              ("/etc/localtime" . "/etc/localtime:ro")))
           (networks "ttrss"))
          (oci-container-configuration
           (image postgres-image)
           (provision "database.postgres")
           (environment
            `(("POSTGRES_PASSWORD" . ,db-password)))
           (volumes
            `((,datadir . "/var/lib/postgresql/data")
              ("/etc/timezone" . "/etc/timezone:ro")
              ("/etc/localtime" . "/etc/localtime:ro")))
           (networks "ttrss"))
          (oci-container-configuration
           (image %mercury-image)
           (provision "service.mercury")
           (networks "ttrss")))))

(define oci-awesome-ttrss-service-type
  (service-type
   (name 'awesome-ttrss)
   (extensions (list (service-extension oci-container-service-type
                                        awesome-ttrss-configuration->oci-container-configuration)
                     (service-extension account-service-type
                                        %awesome-ttrss-accounts)
                     (service-extension activation-service-type
                                        %awesome-ttrss-activation)))
   (default-value (awesome-ttrss-configuration))
   (description "This service install a OCI backed Forgejo Shepherd Service.")))
