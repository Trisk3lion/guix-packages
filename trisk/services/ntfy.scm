(define-module (trisk services ntfy)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (trisk packages binaries)
  #:export (ntfy-service-type
            ntfy-configuration))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)))

;; (define (strings? lst)
;;   (every string? lst))

(define (serialize-field field-name value)
  #~(string-append #$(uglify-field-name field-name)
                   ": "
                   #$value
                   "\n"))

;; $#(format #f "~a: ~a~%" #$field-name #$value)

(define (serialize-string field-name value)
  (serialize-field field-name (format #f "\"~s\""value)))

(define (serialize-symbol field-name value)
  (serialize-field field-name (symbol->string value)))

(define (serialize-integer field-name value)
  (serialize-field field-name (number->string value)))

(define (serialize-boolean field-name value)
  (serialize-string field-name (if value "true" "false")))

(define (serialize-ntfy-configuration configuration)
  (mixed-text-file
   "ntfy.conf"
   #~(string-append #$(serialize-configuration
                       configuration ntfy-configuration-fields))))

(define (sql-query? value)
  (and (string? value) (string-suffix? ";" value)))

(define (sql-queries? value)
  (every sql-query? value))

(define (serialize-sql-queries field-name strings)
  (serialize-field field-name (format #f "|~%~{~4_~s~%~}" strings)))

(define (serialize-sql-queries-test field-name strings)
  (serialize-field field-name (if (eq? strings '())
                                  ""
                                  (format #f "|~%~{~4_~s~%~}" strings))))

(define-maybe integer)
(define-maybe string)

(define-configuration ntfy-configuration
  (ntfy
   (file-like ntfy-bin)
   "ntfy package to use.")
  (base-url
   maybe-string
   "Public facing base URL of the service

    This setting is required for any of the following features:
     - attachments (to return a download URL)
     - e-mail sending (for the topic URL in the email footer)
     - iOS push notifications for self-hosted servers
       (to calculate the Firebase poll_request topic)
     - Matrix Push Gateway (to validate that the pushkey is correct)")
  (listen-http
   maybe-string
   "Listen address for the HTTP web server")
  (listen-https
   maybe-string
   "Listen address for the HTTPS web server. If set, you also need to set key-file and cert-file.")
  (listen-unix
   maybe-string
   "Path to a Unix socket to listen on.")
  (listen-unix-mode
   maybe-string
   "File mode of the Unix socket, e.g. 0700 or 0777")
  (behind-proxy?
   (boolean #f)
   "Whatever ntfy is behind a proxy or not.")
  (key-file
   maybe-string
   "HTTPS/TLS private key file, only used if listen-https is set.")
  (cert-file
   maybe-string
   "HTTPS/TLS certificate file, only used if listen-https is set.")
  (cache-file
   (string "/var/cache/ntfy/cache.db")
   "Doc...")
  (attachment-cache-dir
   (string "/var/cache/ntfy/attachment")
   "Doc...")
  (log-format
   (string "text")
   "Log format, text or json.")
  (log-level
   (symbol 'info)
   "Log level; either 'log, 'error 'debug.")
   (log-file
    (string "/var/log/ntfy.log")
    "File to log to."
    empty-serializer)
   (cache-batch-size
    maybe-integer
    "Number of messages to batch write to the DB")
   (cache-batch-timeout
    maybe-string
    "Cache batche timeout")
    (cache-startup-queries
     (sql-queries '("pragma journal_mode = WAL;"
                    "pragma synchronous = normal;"
                    "pragma temp_store = memory;"
                    "pragma busy_timeout = 15000;"
                    "vacuum;"))
     "SQL Queries to run on the SQLite database on startup,
for example to activate Write-ahead-log (WAL). "))

;; (define (serialize-ntfy-configuration field-name val)
;;   (define serialize-field
;;     (match-lambda
;;       ((key . value)
;;        (format #f "~a: ~a" key value))
;;       (e e)))

;;   #~(string-append
;;      #$@(interpose (map serialize-field val) "\n" 'suffix)))

(define (ntfy-activation config)
  (match-config <ntfy-configuration> config
                (cache-file attachment-cache-dir))
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "ntfy"))
              (cache-dir (dirname $#cache-file)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir (passwd:uid user)
                             (passwd:gid user)))
                    (list cache-dir
                          #$attachment-cache-dir))))))

(define %ntfy-accounts
  (list (user-group
         (name "ntfy")
         (system? #t))
        (user-account
         (name "ntfy")
         (group "ntfy")
         (system? #t)
         (comment "ntfy User")
         (home-directory "/var/ntfy")
         (shell (file-append shadow "/sbin/nologin")))))

;; (define (ntfy-account config)
;;   (list (user-group
;; 	 (name (ntfy-configuration-group config))
;; 	 (system? #t))
;; 	(user-account
;; 	 (name (ntfy-configuration-user config))
;; 	 (group (ntfy-configuration-group config))
;; 	 (home-directory "/var/empty")
;; 	 (shell (file-append shadow "/sbin/nologin"))
;; 	 (system? #t))))


(define (ntfy-shepherd-service config)
  (match-record config <ntfy-configuration>
                (ntfy base-url)
    (shepherd-service
      (documentation "nftfy notification service")
      (provision '(ntfy))
      (requirement '(user-processes networking))
      (start #~(make-forexec-constructor
                (list #$(file-append ntfy "/bin/ntfy")
                      "serve"
                      "--base-url" #$base-url
                      "--cache-file" "/var/cache/ntfy/cache.db"
                      "--cache-startup-queries" "pragma journal_mode = WAL;"
                      "--attachment-cache-dir" "/var/cache/ntfy/attachments"))))))

(define ntfy-service-type
  (service-type
   (name 'ntfy)
   (description "Run the ntfy service.")
   (extensions
    (list (service-extension account-service-type
                             (const %ntfy-accounts))
          (service-extension shepherd-root-service-type
                             ntfy-shepherd-service)
          (service-extension activation-service-type
                             ntfy-activation)))
   (default-value ntfy-configuration)))
