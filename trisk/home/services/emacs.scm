(define-module (trisk home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:autoload   (gnu packages emacs) (emacs emacs-minimal)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu services configuration)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:export (home-emacs-configuration
            home-emacs-service-type))

(define list-of-string?
  (list-of string?))

(define list-of-file-likes?
  (list-of file-like?))

(define file-like-or-#f?
  (match-lambda
    ((or #f (? file-like?)) #t)
    (_ #f)))

(define-configuration/no-serialization home-emacs-configuration
  (servers
   (list-of-string (list "server"))
   "List of strings which name Emacs servers to run.")
  (emacs
   (file-like emacs)
   "The package providing @file{/bin/emacs}.")
  (packages
   (list-of-file-likes '())
   "Packages to add to the Emacs load path.")
  (native-compile?
   (boolean #f)
   "Whether to enable native-compilation of Emacs packages by building them with
@code{emacs} rather than @code{emacs-minimal}.  Has no effect if
COMPILATION-EMACS is not set to #F.")
  (compilation-emacs
   (file-like-or-#f #f)
   "The Emacs package to use to compile Emacs packages.  If #F, default to
EMACS-MINIMAL.")
  (init-file
   (file-like (plain-file "init.el" ""))
   "File-like object to use as the Emacs initialisation script.")
  (early-init-file
   (file-like (plain-file "early-init.el" ""))
   "File-like object to use as the Emacs early-initialisation script.")
  (custom-file
   (string "custom.el")
   "Name of a file within the Emacs configuration directory to which variables
set by Custom will be written.")
  (debug?
   (boolean #f)
   "Whether to enable debug output from Emacs."))

(define (emacs-for-compile config)
  (match-record config <home-emacs-configuration> (native-compile?
                                                   compilation-emacs)
    (cond
     (compilation-emacs compilation-emacs)
     (native-compile? emacs)
     (else emacs-minimal))))

(define (transformed-emacs-packages config)
  (match-record config <home-emacs-configuration> (packages)
    (map (package-input-rewriting
          `((,emacs-minimal . ,(emacs-for-compile config))))
         packages)))

(define (make-emacs-profile config)
  (match-record config <home-emacs-configuration> (packages)
    (profile
     (name "emacs-profile")
     (content (packages->manifest (cons (emacs-for-compile config)
                                        packages))))))

(define (build-emacs-profile config)
  (with-store store
    (run-with-store store
      (mlet %store-monad
          ((profile-drv (lower-object
                         (make-emacs-profile config))))
        (mbegin %store-monad
          (built-derivations (list profile-drv))
          (return (derivation-output-path
                   (assoc-ref (derivation-outputs profile-drv)
                              "out"))))))))

(define (emacs-environment config)
  #~(map (lambda (env-var)
           (let ((variable (list-ref env-var 0))
                 (separator (list-ref env-var 1))
                 (value (list-ref env-var 2)))
             (string-append variable "="
                            (or (and=> (getenv variable)
                                       (lambda (original)
                                         (string-append original
                                                        separator)))
                                "")
                            value)))
         '(#$@(map (match-lambda
                     ((spec . value)
                      (match-record spec <search-path-specification>
                          (variable separator)
                        (list variable separator value))))
                   (profile-search-paths
                    (build-emacs-profile config))))))

(define (home-emacs-profile-packages config)
  (match-record config <home-emacs-configuration> (emacs)
    (list emacs)))

(define (home-emacs-shepherd-services config)
  (match-record config <home-emacs-configuration>
      (servers emacs custom-file debug?)
    (map (lambda (server)
           (shepherd-service
            (provision (list (string->symbol
                              (string-append "emacs-" server))))
            (documentation
             (string-append "Start the Emacs server called "
                            server "."))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append emacs "/bin/emacs")
                      (string-append "--fg-daemon=" #$server)
                      "--eval"
                      (format #f "~s"
                              (let ((custom-file
                                     (format #f "~a/emacs/custom.el"
                                             (or (getenv "XDG_CONFIG_HOME")
                                                 (format #f "~a/.config"
                                                         (getenv "HOME"))))))
                                `(when (file-readable-p ,custom-file)
                                   (setq custom-file ,custom-file)
                                   (load custom-file))))
                      #$@(if debug?
                             (list "--debug-init")
                             '()))
                #:log-file
                (format #f "~a/log/emacs/~a.log"
                        (or (getenv "XDG_STATE_HOME")
                            (format #f "~a/.local/state"
                                    (getenv "HOME")))
                        #$server)
                #:environment-variables
                (append (default-environment-variables)
                        #$(emacs-environment config))))
            (stop
             #~(make-forkexec-constructor
                (list #$(file-append emacs "/bin/emacsclient")
                      "-s" #$server "--eval" "(kill-emacs)")))))
         servers)))

(define (home-emacs-xdg-configuration-files config)
  (match-record config <home-emacs-configuration> (early-init-file
                                                   init-file)
    `(("emacs/early-init.el"
       ,(home-emacs-configuration-early-init-file config))
      ("emacs/init.el"
       ,(home-emacs-configuration-init-file config)))))

(define home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (extensions
    (list (service-extension
           home-profile-service-type
           home-emacs-profile-packages)
          (service-extension
           home-shepherd-service-type
           home-emacs-shepherd-services)
          (service-extension
           home-xdg-configuration-files-service-type
           home-emacs-xdg-configuration-files)))
   (default-value (home-emacs-configuration))
   (description
    "Configure and run the GNU Emacs extensible text editor.")))
