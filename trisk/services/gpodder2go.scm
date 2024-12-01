(define-module (trisk services gpodder2go)
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
  #:use-module (trisk packages gpodder)
  #:export (gpodder2go-service-type
	    gpodder2go-configuration
	    gpodder2go-configuration?))

;; (define (initial-string? val) (string? val))
;; (define (initial-boolean? val) (boolean? val))
;; (define (strip-initial-prefix field-name) (string-drop (symbol->string field-name) 8))

;; (define (serialize-string field-name val)
;;   (format #f "~a=~a\n" (string-upcase
;; 			(string-replace-substring
;; 			 (if (symbol? field-name) (symbol->string field-name) field-name) "-" "_"))
;; 	  val))

;; (define (serialize-boolean field-name val)
;;   (if val (serialize-string field-name "1") (serialize-string field-name "0")))

;; ;; Initial string is just a string with 'initial-' prefix
;; (define (serialize-initial-string field-name val)
;;   (serialize-string (strip-initial-prefix field-name) val))

;; (define (serialize-initial-boolean field-name val)
;;   (serialize-boolean (strip-initial-prefix field-name) val))

(define-configuration/no-serialization gpodder2go-configuration
  (gpodder2go
   (file-like gpodder2go)
   "gpodder2gp package to use.")
  (no-auth?
   (boolean #f)
   "Disale authentication from the gpodder2go server. Needed when connecting with a Gpodder client.")
  (run-migrations
   (boolean #t)
   "Run database migrations during application startup.")
  (log-file
   (string "/var/log/gpodder2go.log")
   "Path to the log file"))

(define (gpodder2go-shepherd-service config)
  (let* ((config-file (mixed-text-file "gpodder2go.conf"
                                       (serialize-configuration config gpodder2go-configuration-fields))))
    (list (shepherd-service
	   (documentation "Run Miniflux server")
	   (provision '(gpodder2go))
	   (requirement '(networking postgresql))
	   (start #~(make-forkexec-constructor
		     (list (string-append #$gpodder2go "/bin/gpodder2go")
			   "-config-file" #$config-file)
		     #:user #$(gpodder2go-configuration-user config)
		     #:group #$(gpodder2go-configuration-group config)
		     #:log-file #$(gpodder2go-configuration-log-file config)))
	   (stop #~(make-kill-destructor))))))

;; (define (gpodder2go-account config)
;;   (list (user-group
;; 	 (name (gpodder2go-configuration-group config))
;; 	 (system? #t))
;; 	(user-account
;; 	 (name (gpodder2go-configuration-user config))
;; 	 (group (gpodder2go-configuration-group config))
;; 	 (home-directory "/var/empty")
;; 	 (shell (file-append shadow "/sbin/nologin"))
;; 	 (system? #t))))

(define (gpodder2go-activation config)
  (match-record config
                <gpodder2go-configuration>
                (gpodder2go)
   #~(begin
       (use-modules (guix build utils))

       (display "Initiating gpodder2go database")
       (system* (string-append #$gpodder2go "/bin/gpodder2go")
                "init"))))

(define gpodder2go-service-type
  (service-type
   (name 'gpodder2go)
   (description "gpodder2go service")
   (default-value (gpodder2go-configuration))
   (extensions
    (list ;; (service-extension account-service-type
     ;;       	     gpodder2go-account)
     (service-extension shepherd-root-service-type
			gpodder2go-shepherd-service)
     (service-extension activation-service-type
                        gpodder2go-activation)))))
