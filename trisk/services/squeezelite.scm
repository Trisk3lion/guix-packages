(define-module (trisk services squeezelite)
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
  (pid-file
   (string "/var/run/squeezelite.pid")
   "Pid-file")
  (name?
   maybe-string
   "Name of the squeezelite instance.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define squeezelite-shepherd-service config
  (match-record-lambda <squeezelite-configuration>
      (squeezelite output-device pid-file extra-options)
    (list (shepherd-service
           (documentation "Run squeezelite")
           (provision '(sqeezelite))
           (requirement '(alsa))
           (start #~(make-forkexec-constructor
                     (list #$(file-append squeezelite "/bin/squeezelite")
                           "-o" #$output-device
                           "-P" #$pid-file
                           #$@(if (maybe-value-set? name?)
                                  '()
                                  '("-n" #$name))
                           #$@extra-options)
                     #:pid-file #$pid-file
                     #:log-file "/var/log/squeezelite.log")
                  (stop #~(make-kill-destructor)))))))

(define squeezelite-service-type
  (service-type
   (name 'squeezelite)
   (extensions
    (list (service-extension shepherd-root-service-type
                             squeezelite-shepherd-service)))))