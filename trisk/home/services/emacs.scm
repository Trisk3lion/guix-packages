(define-module (trisk home services emacs)
  #:use-module ((gnu services configuration)
                #:select (serialize-package define-configuration
                          define-configuration/no-serialization text-config?
                          serialize-text-config maybe-value-set?))
  #:use-module ((gnu packages emacs)
                #:select (emacs))
  #:use-module ((gnu home services)
                #:select (service-type service-extension
                                       home-profile-service-type
                                       home-files-service-type))
  #:use-module ((gnu home services shepherd)
                #:select (shepherd-service home-shepherd-service-type))
  #:use-module ((guix gexp)
                #:select (gexp file-append local-file mixed-text-file))
  #:use-module ((guix packages)
                #:select (package?))

  #:export (home-emacs-service-type home-emacs-configuration))

;;; Emacs

(define-configuration/no-serialization home-emacs-extension
  (configurations
   (text-config '())
   "The configuration for the extension."))

(define-configuration home-emacs-configuration
  (package
    (package emacs)
    "Package to use for setting Emacs")
  (configurations
   (text-config '())
   "A list of other configuration files to autoload"))

(define (add-emacs-packages config)
  (list (home-emacs-configuration-package config)))

(define (home-emacs-extensions original-config extension-configs)
  (home-emacs-configuration (inherit original-config)
                            (configurations (apply append
                                                   (home-emacs-configuration-configurations
                                                    original-config)
                                                   (map
                                                    home-emacs-extension-configurations
                                                    extension-configs)))))

(define (home-emacs-shepherd-service config)
  (list (shepherd-service (documentation "Start Emacs")
                          (provision '(emacs))
                          (auto-start? #t)
                          (start #~(make-forkexec-constructor (list #$(file-append
                                                                       (home-emacs-configuration-package
                                                                        config)
                                                                       "/bin/emacs")
                                                               "--fg-daemon")
                                                              #:log-file (format
                                                                          #f
                                                                          "~a/.local/var/log/xbindkeys.log"
                                                                          (getenv
                                                                           "HOME"))))
                          (stop #~(make-kill-destructor)))))

(define (home-emacs-config-files config)
  `((".emacs.d/init.el" ,(mixed-text-file "init.el"
                                          (serialize-text-config
                                           config
                                           (home-emacs-configuration-configurations
                                                                  config))))))

(define home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (extensions (list
                (service-extension
                 home-profile-service-type
                 add-emacs-packages)
                (service-extension
                 home-shepherd-service-type
                 home-emacs-shepherd-service)
                (service-extension
                 home-files-service-type
                 home-emacs-config-files)))
   (compose identity)
   (extend home-emacs-extensions)
   (default-value (home-emacs-configuration))
   (description "Install and configure Emacs.")))
