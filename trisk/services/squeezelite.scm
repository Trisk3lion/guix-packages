(define-module (trisk services squeezelite)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (trisk packages audio)
  #:use-module (ice-9 string-fun)
  #:export (squeezelite-service-type
	    squeezelite-configuration
	    squeezelite-configuration?))

(define-maybe/no-serialization string)

(define-configuration squeezelite-configuration
  (squeezelite
   (file-like squeezelite)
   "The squeezelite package to use.")
  (output-device
   (string "default")
   "The output device to use.")
  (user
   (user-account %squeezelite-user)
   "User to run the squeezelite service.")
  (group
   (user-group %squeezelite-group)
   "Group to run the squeezelite service.")
  (environment-variables
   (list-of-strings '("PULSE_CLIENTCONFIG=/etc/pulse/client.conf"
                      "PULSE_CONFIG=/etc/pulse/daemon.conf"))
   "A list of strings specifying environment variables.")
  ;; (pid-file
  ;;  (string "/var/run/squeezelite.pid")
  ;;  "Pid-file")
  (log-file
   (string "/var/log/squeezelite.log")
   "Log file path.")
  (name
   maybe-string
   "Name of the squeezelite instance.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define squeezelite-log-rotations
  (match-record-lambda <squeezelite-configuration>
      (log-file)
    (list (log-rotation
           (files (list log-file))))))

(define %squeezelite-user
  (user-account
   (name "squeezelite")
   (group %squeezelite-group)
   (comment "Squeezelite user")
   (home-directory "/var/lib/squeezelite")
   (shell (file-append shadow "/sbin/nologin"))
   (supplementary-groups '("audio"))
   (system? #t)))

(define %squeezelite-group
  (user-group
   (name "squeezelite")
   (system? #t)))

(define (squeezelite-account config)
  (match-record config <squeezelite-configuration>
                (list user group)))

;; (define (squeezelite-activation config)
;;   "Create the necessary directories for tailscale and run 'squeezelite
;; --cleanup' at startup, as recommended."
;;   (with-imported-modules '((guix build utils))
;;     #~(begin
;;         (use-modules (guix build utils))
;;         (mkdir-p (dirname #$(squeezelite-configuration-state-directory config)))
;;         (mkdir-p (dirname #$(squeezelite-configuration-socket config)))
;;         (system* #$(file-append (squeezelite-configuration-tailscale config)
;;                                 "/sbin/squeezelite") "--cleanup"))))

(define squeezelite-shepherd-service 
  (match-record-lambda <squeezelite-configuration>
      (squeezelite output-device user group environment-variables ;; pid-file
                   name log-file extra-options)
    (let ((username (user-account-name user))
          (groupname (user-group-name group)))
      (list (shepherd-service
             (documentation "Run squeezelite")
             (provision '(squeezelite))
             (requirement '(user-processes networking))
             (start
              #~(begin
                  (let ((home #$(user-account-home-directory user)))
                    (let ((user (getpw #$username))
                          (group (getgr #$groupname))))
                    (make-forkexec-constructor
                     (list #$(file-append squeezelite "/bin/squeezelite")
                           "-o" #$output-device
                           "-d" "all=info"
                           ;; "-P" #$pid-file
                           #$@(if (maybe-value-set? name)
                                  (list "-n" name)
                                  '())
                           #$@extra-options)
                     #:user user
                     #:group group
                     #:environment-variables
                     ;; Set HOME so MPD can infer default paths, such as
                     ;; for the database file.
                     (cons (string-append "HOME=" home)
                           '#$environment-variables)
                     ;; #:pid-file #$pid-file
                     #:log-file #$log-file))))
                    (stop #~(make-kill-destructor)))))))

(define squeezelite-service-type
  (service-type
   (name 'squeezelite)
   (description "Squeezelite service")
   (default-value (squeezelite-configuration))
   (extensions
    (list (service-extension account-service-type
			     squeezelite-account)
          (service-extension shepherd-root-service-type
                             squeezelite-shepherd-service)
          (service-extension rottlog-service-type
                             squeezelite-log-rotations)))))
