(define-module (trisk services docker-tailscale)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:export (docker-tailscale-configuration
            docker-tailscale-service-type))

(define-maybe string)

(define-configuration docker-tailscale-configuration
  (docker-image
   (string "almeidapaulopt/tsdproxy:latest")
   "Docker image to use.")
  (data-directory
   (string "/opt/docker-tailscale/data")
   "Path to data directory.")
  (config-directory
   (string "/opt/docker-tailscale/config")
   "Path to configuration directory.")
  (server-hostname
   (string "192.168.0.10")
   "IP address of the docker server.")
  (auth-key
   (string "")
   "Auth-key to authenticate to Tailscale server.")
  (control-url
   (string "https://controlplane.tailscale.com")
   "URL to the control server.")
  (log-file
   (string "/var/log/docker-tailscale.log")
   "Path to log file.")
  (requirement
   (list-of-symbols '())
   "List of symbols for other shepherd services which we require.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define %docker-tailscale-accounts
  (list (user-account
         (name "docker-tailscale")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define docker-tailscale-log-rotations
  (match-record-lambda <docker-tailscale-configuration>
      (log-file)
    (list (log-rotation
           (files (list log-file))))))

(define docker-tailscale-activation
  (match-record-lambda <docker-tailscale-configuration>
      (data-directory config-directory)
    #~(let ((user (getpwnam "docker-tailscale")))
        (for-each
         (lambda (directory)
           (unless (file-exists? directory)
             (mkdir-p directory)
             (chown directory (passwd:uid user) (passwd:gid user))))
         '#$(list data-directory config-directory)))))

(define docker-tailscale-oci-containers
  (match-record-lambda <docker-tailscale-configuration>
      (docker-image data-directory config-directory server-hostname control-url log-file requirement extra-options)
    (list (oci-container-configuration
           (user "docker-tailscale")
           (group "docker")
           (environment
            `(("TSDPROXY_AUTHKEY" . ,authkey)
              ("TSDPROXY_HOSTNAME" . ,server-hostname)
              ("TSDPROXY_CONTROLURL" . ,control-url)
              ("TSDPROXY_DATADIR" . "/data/")
              ("DOCKER_HOST" . "unix:///var/run/docker.sock")))
           (image docker-image)
           (provision "docker-tailscale")
           (log-file log-file)
           (respawn? #t)
           (requirement requirement)
           (volumes
            `(("/var/run/docker.sock" . "/var/run/docker.sock")
              (,data-directory . "/data")
              (,config-directory . "/config")))
           (extra-arguments extra-options)))))

(define docker-tailscale-service-type
  (service-type
   (name 'docker-tailscale)
   (extensions
    (list (service-extension account-service-type
                             (const %docker-tailscale-accounts))
          (service-extension activation-service-type
                             docker-tailscale-activation)
          (service-extension rottlog-service-type
                             docker-tailscale-log-rotations)
          (service-extension oci-container-service-type
                             docker-tailscale-oci-containers)))
   (default-value (docker-tailscale-configuration))
   (description "Run docker-tailscale proxy, TSDProxy.")))
