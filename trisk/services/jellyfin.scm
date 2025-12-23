(define-module (trisk services jellyfin)
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
  #:export (jellyfin-configuration
            jellyfin-service-type))

(define-maybe string)

(define-configuration jellyfin-configuration
  (cache-directory
   (string "/var/cache/jellyfin")
   "Path to cache directory.")
  (config-directory
   (string "/var/lib/jellyfin")
   "Path to configuration directory.")
  (proxy-url
   maybe-string
   "Proxy URL.")
  (log-file
   (string "/var/log/jellyfin.log")
   "Path to log file.")
  (requirement
   (list-of-symbols '())
   "List of symbols for other shepherd services which we require.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define %jellyfin-accounts
  (list (user-account
         (name "jellyfin")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define jellyfin-log-rotations
  (match-record-lambda <jellyfin-configuration>
      (log-file)
    (list log-file)))

(define jellyfin-activation
  (match-record-lambda <jellyfin-configuration>
      (cache-directory config-directory)
    #~(let ((user (getpwnam "jellyfin")))
        (for-each
         (lambda (directory)
           (unless (file-exists? directory)
             (mkdir-p directory)
             (chown directory (passwd:uid user) (passwd:gid user))))
         '#$(list cache-directory config-directory)))))

(define jellyfin-oci-containers
  (match-record-lambda <jellyfin-configuration>
      (cache-directory config-directory proxy-url log-file requirement extra-options)
    (oci-extension
     (containers
      (list (oci-container-configuration
           (user "jellyfin")
           (group "docker")
           (environment
            (if (maybe-value-set? proxy-url)
                `(("http_proxy" . ,proxy-url)
                  ("https_proxy" . ,proxy-url))
                '()))
           (image "jellyfin/jellyfin:latest")
           (provision "jellyfin")
           (log-file log-file)
           (respawn? #t)
           (requirement requirement)
           (network "host")
           (volumes
            `((,cache-directory . "/cache")
              (,config-directory . "/config")))
           (extra-arguments extra-options)))))))

(define jellyfin-service-type
  (service-type
   (name 'jellyfin)
   (extensions
    (list (service-extension account-service-type
                             (const %jellyfin-accounts))
          (service-extension activation-service-type
                             jellyfin-activation)
          (service-extension log-rotation-service-type
                             jellyfin-log-rotations)
          (service-extension oci-service-type
                             jellyfin-oci-containers)))
   (default-value (jellyfin-configuration))
   (description "Run Jellyfin, a media system.")))
