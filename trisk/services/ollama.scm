(define-module (trisk services ollama)
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
  #:use-module (trisk packages ai)
  #:export (ollama-service-type
            ollama-configuration))

(define-configuration/no-serialization ollama-configuration
  (ollama
   (file-like ollama)
   "Ollama")
  (user
   (string "ollama")
   "Username.")
  (group
   (string "ollama")
   "Group.")
  (log-file
   (string "/var/log/ollama.log")
   "log-file."))


(define (ollama-log-rotations config)
  (list (ollama-configuration-log-file config)))

(define (ollama-accounts config)
  (match-record config <ollama-configuration>
                (user group)
    (list (user-group
            (name group)
            (system? #t))
          (user-account
            (name user)
            (group group)
            (system? #t)
            (comment "ollama server user")
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

(define ollama-shepherd-service
  (match-record-lambda <ollama-configuration>
      (ollama user group log-file)
    (list (shepherd-service
            (documentation "Run ollama")
            (provision '(ollama))
            (requirement '(user-processes networking))
            (start #~(make-forkexec-constructor
                      (list
                       #$(file-append ollama "/bin/ollama")
                       "serve")
                      #:user #$user
                      #:group #$group
                      #:log-file #$log-file))
            (stop #~(make-kill-destructor))))))

(define ollama-service-type
  (service-type
   (name 'ollama)
   (extensions
    (list (service-extension account-service-type
			      ollama-accounts)
          (service-extension shepherd-root-service-type
                             ollama-shepherd-service)
          ;; (service-extension activation-service-type
          ;;                    ollama-activation)
          (service-extension log-rotation-service-type
                             ollama-log-rotations)))
   (default-value (ollama-configuration))
   (description "Run ollama.")))
