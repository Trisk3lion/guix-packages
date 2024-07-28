(define-module (trisk services miniflux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (gnu services databases)
  #:use-module (gnu packages web)
  #:use-module (ice-9 string-fun)
  #:export (miniflux-service-type
	    miniflux-configuration
	    miniflux-configuration?))

(define (initial-string? val) (string? val))
(define (initial-boolean? val) (boolean? val))
(define (strip-initial-prefix field-name) (string-drop (symbol->string field-name) 8))

(define (serialize-string field-name val)
  (format #f "~a=~a\n" (string-upcase
			(string-replace-substring
			 (if (symbol? field-name) (symbol->string field-name) field-name) "-" "_"))
	  val))

(define (serialize-boolean field-name val)
  (if val (serialize-string field-name "1") (serialize-string field-name "0")))

;; Initial string is just a string with 'initial-' prefix
(define (serialize-initial-string field-name val)
  (serialize-string (strip-initial-prefix field-name) val))

(define (serialize-initial-boolean field-name val)
  (serialize-boolean (strip-initial-prefix field-name) val))

(define-configuration miniflux-configuration
  (listen-addr
   (string "127.0.0.1:8080")
   "Address to listen on. Use absolute path for a Unix socket.")
  (base-url
   (string "http://localhost/")
   "Base URL to generate HTML links and base path for cookies.")
  (initial-create-admin
   (initial-boolean #t)
   "Create an initial admin")
  (initial-admin-username
   (initial-string "admin")
   "Initial admin username")
  (initial-admin-password
   (initial-string "password")
   "Initial admin password")
  (user
   (string "miniflux")
   "User name for Postgresql and system account")
  (database-url
   (string "host=/var/run/postgresql")
   "PostgreSQL connection string")
  (group
   (string "miniflux")
   "Group for the system account"
   empty-serializer)
  (run-migrations
   (boolean #t)
   "Run database migrations during application startup.")
  (log-file
   (string "/var/log/miniflux.log")
   "Path to the log file"
   empty-serializer))

(define (miniflux-shepherd-service config)
  (let* ((config-file (mixed-text-file "miniflux.conf"
                                       (serialize-configuration config miniflux-configuration-fields))))
    (list (shepherd-service
	   (documentation "Run Miniflux server")
	   (provision '(miniflux))
	   (requirement '(networking postgresql))
	   (start #~(make-forkexec-constructor
		     (list (string-append #$miniflux "/bin/miniflux")
			   "-config-file" #$config-file)
		     #:user #$(miniflux-configuration-user config)
		     #:group #$(miniflux-configuration-group config)
		     #:log-file #$(miniflux-configuration-log-file config)))
	   (stop #~(make-kill-destructor))))))

(define (miniflux-account config)
  (list (user-group
	 (name (miniflux-configuration-group config))
	 (system? #t))
	(user-account
	 (name (miniflux-configuration-user config))
	 (group (miniflux-configuration-group config))
	 (home-directory "/var/empty")
	 (shell (file-append shadow "/sbin/nologin"))
	 (system? #t))))

(define (miniflux-postgresql-role config)
  (list (postgresql-role
	 (name (miniflux-configuration-user config))
         (create-database? #t))))

(define miniflux-service-type
  (service-type
   (name 'miniflux)
   (description "Miniflux service")
   (default-value (miniflux-configuration))
   (extensions
    (list (service-extension account-service-type
			     miniflux-account)
	  ;; (service-extension postgresql-service-type (const #t))
	  (service-extension postgresql-role-service-type
			     miniflux-postgresql-role)
	  (service-extension shepherd-root-service-type
			     miniflux-shepherd-service)))))
