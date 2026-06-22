(define-module (trisk services calibre-server)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ebook)
  #:use-module (ice-9 string-fun)
  #:export (calibre-server-service-type
	    calibre-server-configuration
	    calibre-server-configuration?))

(define-maybe string)

(define-configuration/no-serialization calibre-server-configuration
  (calibre
   (file-like calibre)
   "Calibre package to use for the Calibre Content Server")
  (libraries
   (list-of-strings '())
   "List of paths to the calibre libaries.
It is important that the user running calibre-server has read access to these libaries.")
  (url-prefix
   maybe-string
   "Serve all URL's prefixed by url-prefix.")
  (interface
   (string "0.0.0.0")
   "The interface which to listen to connections, for example: ::1 or 0.0.0.0.")
  (port
   (number 8080)
   "The port which to listen to connections.")
  (user
   (string "calibre")
   "User name for the user that should run the server. Should be the same user that owns the files in calibre library folder.")
  (group
   (string "calibre")
   "Group name for the group that should run the server. Should be the same group that owns the files in calibre library folder.")
  (enable-auth
   (boolean #f)
   "Enable password based authentication to the server.")
  (auth-mode
   (string "auto")
   "Type of authntication method if used, either:
 - auto (default)
 - basic
 - digest
")
  (userdb
   maybe-string
   "User database file for authentication, needs to be initialized before hand.")
  (trusted-ips
   maybe-string
   "Allow un-authenticated connections from specific IP addresses to make changes.
Should be a comma separated list of address or network specifications.")
  (pid-file
   (string "/var/log/calibre-server.pid")
   "Path to PID-file.")
  (log-file
   (string "/var/log/calibre-server.log")
   "Path to the log file")
  (extra-flags
   (list-of-strings '())
   "Extra flags as a list of strings"))

(define (calibre-server-accounts config)
  (list (user-group
         (system? #t)
         (name "calibre"))
        (user-account
         (name "calibre")
         (comment "Calibre Server Service Account")
         (group "calibre")
         (supplementary-groups '("tty"))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

;; (define (calibre-server-activation config)
;;   (match-record config <calibre-server-configuration>
;;       (user group library-path)
;;   #~(begin
;;         (use-modules (guix build utils)
;;         (let ((user (getpwnam "calibre"))
;;               (uid (passwd:uid user))
;;               (gid (passwd:gid user))
;;               (dir #$library-path))
;;           ;; Setup datadir
;;           (unless (file-exists? dir)
;;             (mkdir-p dir)
;;             (chown datadir uid gid)
;;             (chmod datadir #o770)))))))

(define (calibre-server-shepherd-service config)
  (match-record config <calibre-server-configuration>
    (calibre url-prefix enable-auth auth-mode user group
             interface trusted-ips userdb log-file libraries port extra-flags)
    (list (shepherd-service
	   (documentation "Run Calibre Content Server")
	   (provision '(calibre-server))
	   (requirement '(networking))
	   (start #~(make-forkexec-constructor
		     (list (string-append #$calibre "/bin/calibre-server")
                           "--listen-on" #$interface
                           "--port" #$(number->string port)

                           #$@(if (maybe-value-set? url-prefix)
                                  (list "--url-prefix" url-prefix)
                                  '())
                           #$@(if enable-auth
                                  '("--enable-auth")
                                  '("--disable-auth"))
                           "--auth-mode" #$auth-mode
                           #$@(if (maybe-value-set? trusted-ips)
                                  (list "--trusted-ips" trusted-ips)
                                  '())
                           #$@(if (maybe-value-set? userdb)
                                  (list "--userdb" userdb)
                                  '())
                           #$@extra-flags
                           "--" #$@libraries)
		     #:user #$user
		     #:group #$group
		     #:log-file #$log-file))
	   (stop #~(make-kill-destructor))))))

(define (calibre-server-log-rotations config)
  (list (log-rotation
         (files (list (calibre-server-configuration-log-file config)))
         (frequency 'weekly))))

(define calibre-server-service-type
  (service-type
   (name 'calibre-server)
   (description "Calibre Content Server")
   (extensions
    (list (service-extension shepherd-root-service-type
			     calibre-server-shepherd-service)
          (service-extension account-service-type
                             calibre-server-accounts)
          ;; (service-extension activation-service-type
          ;;                    calibre-server-activation)
          (service-extension log-rotation-service-type
                             calibre-server-log-rotations)))
   (default-value (calibre-server-configuration))))
