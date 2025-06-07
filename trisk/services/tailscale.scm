(define-module (trisk services tailscale)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (trisk packages tailscale)
  #:export (tailscaled-service-type
            tailscaled-configuration
            tailscale-up-service-type
            tailscale-up-configuration)
  )

(define-configuration tailscaled-configuration
  (tailscale
   (file-like tailscale-amd64-bin)
   "The tailscale package to use.")

  (iptables
   (file-like iptables-nft)
   "The iptables package to use.")

  (log-file
   (string "/var/log/tailscaled.log")
   "Path to log file.")

  (socket
   (string "/var/run/tailscale/tailscaled.sock")
   "Path of the service UNIX socket.")

  (state-directory
   (string "/var/lib/tailscale")
   "Path to directory for storage of config state, TLS certs, temporary incoming
Taildrop files, etc.  If empty, it's derived from @code{state-file} when
possible.")

  (upload-log?
   (boolean #f)
   "Whether to upload logs or not, technical support is also disabled when set
to #f.")

  (dev-net-tun?
   (boolean #t)
   "Use dev-net-tun")

  (verbosity
   (integer 0)
   "Log verbosity level; 0 is default, 1 or higher are increasingly verbose.")

  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define (tailscaled-activation config)
  "Create the necessary directories for tailscale and run 'tailscaled
--cleanup' at startup, as recommended."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (dirname #$(tailscaled-configuration-state-directory config)))
        (mkdir-p (dirname #$(tailscaled-configuration-socket config)))
        (system* #$(file-append (tailscaled-configuration-tailscale config)
                                "/sbin/tailscaled") "--cleanup"))))

(define (tailscaled-log-rotations config)
  (list (tailscaled-configuration-log-file config)))

(define tailscaled-shepherd-service
  (match-record-lambda <tailscaled-configuration>
      (tailscale iptables log-file socket state-directory
                 upload-log? dev-net-tun? verbosity extra-options)
    (let ((environment
           #~(list (string-append "PATH="
                                  (string-join
                                   '(#$(file-append iptables "/sbin")
                                     #$(file-append iproute "/sbin"))
                                   ":")))))
      (list (shepherd-service
             (documentation "Run tailscaled")
             (provision '(tailscaled))
             (requirement '(user-processes networking))
             (start #~(make-forkexec-constructor
                       (list
                        #$(file-append tailscale "/sbin/tailscaled")
                        #$@(if upload-log?
                               '()
                               '("-no-logs-no-support"))
                        #$@(if dev-net-tun?
                              '()
                              '("--tun=userspace-networking"))
                        "-socket" #$socket
                        "-statedir" #$state-directory
                        "-verbose" #$(number->string verbosity)
                        #$@extra-options)
                       #:environment-variables #$environment
                       #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define tailscaled-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscaled-shepherd-service)
          (service-extension activation-service-type
                             tailscaled-activation)
          (service-extension profile-service-type
                             (compose list tailscaled-configuration-tailscale))
          (service-extension log-rotation-service-type
                             tailscaled-log-rotations)))
   (default-value (tailscaled-configuration))
   (description "Run tailscaled.")))


(define-configuration tailscale-up-configuration
  (tailscale
   (file-like tailscale-amd64-bin)
   "The tailscale package to use.")

  (operator
   (string "")
    "Operator who can control tailscale.")

  (ssh?
   (boolean #f)
   "Accept ssh connnections over tailscale?")

  (subroutes?
   (boolean #f)
   "Accept advertised subroutes?")

  (exit-node?
   (boolean #f)
   "Advertise this node as an exit node?")

  (authkey
   (string "")
   "Authkey used for connection to your tailscale account.")

  (login-server
   (string "https://controlplane.tailscale.com")
   "Base URL of control server")

  (socket
   (string "/var/run/tailscale/tailscaled.sock")
   "Path of the service UNIX socket.")

  (log-file
   (string "/var/log/tailscale-up.log")
   "Path to log file.")

  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define tailscale-up-shepherd-service
  (match-record-lambda <tailscale-up-configuration>
      (tailscale ssh? subroutes? exit-node? authkey operator
                 socket login-server extra-options log-file)
    (list (shepherd-service
           (documentation "Run tailscale up")
           (provision '(tailscale))
           (requirement '(tailscaled))
           (one-shot? #t)
           (start #~(make-forkexec-constructor
                     (list
                      #$(file-append tailscale "/bin/tailscale")
                      "--socket" #$socket
                      "up"
                      "--authkey" #$authkey
                      "--operator" #$operator
                      #$@(if ssh?
                             '("--ssh")
                             '())
                      #$@(if subroutes?
                             '("--accept-routes")
                             '())
                      #$@(if exit-node?
                             '("--advertise-exit-node")
                             '())
                      "--login-server" #$login-server
                      #$@extra-options)
                     #:log-file #$log-file))
           (stop #~(const #f))))))

(define (tailscale-up-log-rotations config)
  (list (tailscale-up-configuration-log-file config)))

(define tailscale-up-service-type
  (service-type
   (name 'tailscale-up)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-up-shepherd-service)
          (service-extension log-rotation-service-type
                             tailscale-up-log-rotations)))
   (default-value (tailscale-up-configuration))
   (description "Run tailscale up")))
