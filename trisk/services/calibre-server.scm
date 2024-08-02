(define-module (trisk services calibre-server)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
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
  (library-path
   string
   "Path to the calibre library which includes the calibre db.")
  (url-prefix
   maybe-string
   "Serve all URL's prefixed by url-prefix.")
  (user
   string
   "User name for the user that should run the server. Should be the same user that owns the files in calibre library folder.")
  (group
   string
   "Group name for the group that should run the server. Should be the same group that owns the files in calibre library folder."
   empty-serializer)
  (enable-auth
   (boolean #f)
   "Enable password basedauthentication to the server.")
  (trusted-ips
   maybe-string
   "Allow un-authenticated connections from specific IP addresses to make changes.
Should be a comma separated list of address or network specifications.")
  (pid-file
   (string "/var/log/calibre-server.pid"))
  (log-file
   (string "/var/log/calibre-server.log")
   "Path to the log file"))

(define (calibre-shepherd-service config)
  (match-record config <calibre-server-configuration>
                (calibre database-path url-prefix enable-auth user group trusted-ips pid-file log-file)
    (list (shepherd-service
	   (documentation "Run Calibre Content Server")
	   (provision '(calibre-server))
	   (requirement '(networking))
	   (start #~(make-forkexec-constructor
		     (list (string-append #$calibre "/bin/calibre-server")
                           #$database-path
                           #$@(if url-prefix
                                  '("--url-prefix" url-prefix)
                                  '())
                           #$@(if enable-auth
                                  '("--enable-auth")
                                  '())
                           #$@(if trusted-ips
                                  '("--trusted-ips" trusted-ips)
                                  '())
			   "--daemonize"
                           "--pid-file" #$pid-file)
		     #:user #$user
		     #:group #$group
                     #:pid-file #$pid-file
		     #:log-file #$log-file))
	   (stop #~(make-kill-destructor))))))

(define (calibre-server-log-rotations config)
  (list (log-rotation
         (files (list (calibre-server-configuration-log-file config)))
         (frequency 'weekly))))

(define calibre-server-service-type
  (service-type
   (name 'calibre-server)
   (description "Calire Content Server")
   (default-value (calibre-server-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
			     calibre-server-shepherd-service)
          (service-extension rottlog-service-type
                             calibre-server-log-rotations)))))
