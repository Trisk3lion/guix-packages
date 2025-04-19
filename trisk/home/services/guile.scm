(define-module (trisk home services guile)
  #:use-module (ice-9 match)
  #:use-module (gnu home services)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)

  #:export (home-guile-service-type home-guile-configuration))

(define-configuration/no-serialization home-guile-configuration
  (guile (file-like guile-3.0-latest)
         "The @code{guile} package to use.")
  (packages (list '())
            "List of guile package symbols to be added.")
  (config (list '())
          "List of strings that make up a @file{.guileinit} configuration."))

;; (define (home-guile-profile-service config)
;;   (list (home-guile-configuration-guile config)))

;; (define (add-guile-packages config)
;;   (append (list (home-guile-configuration-package config))
;;           (if (home-guile-configuration-use-readline config) (list guile-readline) '())
;;           (if (home-guile-configuration-use-colorized config) (list guile-colorized) '())))

(define (add-guile-packages config)
  (append (list (home-guile-configuration-guile config))
          (home-guile-configuration-packages config)))

;; (define (serialize-guile-configuration config)
;;   (serialize-configuration config home-guile-configuration-fields))

;; (define (home-guile-config-file config)
;;   (computed-file "guile"
;;                  #~(call-with-output-file #$output
;;                      (λ (port)
;;                        (display #$(serialize-guile-configuration config) port)))))

(define (home-guile-config-file config)
  (mixed-text-file "guile"
                   #~(string-append #$@(interpose (home-guile-configuration-config
                                                   config) "\n"
                                                  'suffix))))

(define (home-guile-config-files config)
  `((".guile" ,(home-guile-config-file config))))

(define home-guile-service-type
  (service-type (name 'home-guile)
                (extensions
                 (list (service-extension home-files-service-type
                                          home-guile-config-files)
                       (service-extension home-profile-service-type
                                          add-guile-packages)))
                (default-value (home-guile-configuration))
                (description "Configure guile")))
