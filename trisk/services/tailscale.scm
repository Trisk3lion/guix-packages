(define-module (trisk services tailscale)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (trisk packages tailscale)
  #:export (tailscaled-service-type
            tailscaled-configuration)
  )

;; Taken from https://git.sr.ht/~efraim/my-guix/tree/master/item/dfsg/contrib/services/tailscale.scm

(define-record-type* <tailscaled-configuration>
  tailscaled-configuration make-tailscaled-configuration
  tailscaled-configuration?
  (package      tailscaled-configuration-package
                (default tailscale))   ; package
  (listen-port  tailscaled-configuration-listen-port
                (default 41641))     ; number
  (state-file   tailscaled-configuration-state-file
                (default "/var/lib/tailscale/tailscaled.state"))  ; path
  (socket-file  tailscaled-configuration-socket-file
                (default "/var/run/tailscale/tailscaled.sock"))   ; path
  (no-logs?     tailscaled-configuration-no-logs
                (default #f))
  (dev-net-tun? tailscaled-configuration-dev-net-tun
                (default #t))
  (verbosity    tailscaled-configuration-verbosity
                (default 0)))          ; number

(define (tailscaled-activation config)
  "Create the necessary directories for tailscale and run 'tailscaled
--cleanup' at startup, as recommended."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (dirname #$(tailscaled-configuration-state-file config)))
        (mkdir-p (dirname #$(tailscaled-configuration-socket-file config)))
        (system* #$(file-append (tailscaled-configuration-package config)
                                "/sbin/tailscaled") "--cleanup"))))

;; Can this service be limited to /var/lib/tailscale, /var/run/tailscale and /var/log?
(define (tailscaled-shepherd-service config)
  "Return a <shepherd-service> for Tailscaled with CONFIG"
  (match-record config <tailscaled-configuration>
                (package listen-port state-file socket-file no-logs? dev-net-tun? verbosity)
    (list
      (shepherd-service
        (provision '(tailscaled))
        (documentation "Tailscaled networking daemon")
        (requirement '(networking))
        (start #~(make-forkexec-constructor
                   (list #$(file-append package "/sbin/tailscaled")
                         #$@(if dev-net-tun?
                              '()
                              '("--tun=userspace-networking"))
                         "-state" #$state-file
                         "-socket" #$socket-file
                         "-port" (number->string #$listen-port)
                         #$@(if no-logs?
                              '("-no-logs-no-support")
                              '())
                         "-verbose" (number->string #$verbosity))
                   #:log-file "/var/log/tailscaled.log"))
        (stop #~(make-kill-destructor))))))

(define %tailscaled-log-rotation
  (list (log-rotation
          (files '("/var/log/tailscaled.log"))
          (options `("rotate 4"
                     ,@%default-log-rotation-options)))))

(define tailscaled-service-type
  (service-type
    (name 'tailscaled)
    (extensions
      (list (service-extension shepherd-root-service-type
                               tailscaled-shepherd-service)
            (service-extension activation-service-type
                               tailscaled-activation)
            (service-extension rottlog-service-type
                               (const %tailscaled-log-rotation))
            (service-extension profile-service-type
                               (compose list tailscaled-configuration-package))))
    (default-value (tailscaled-configuration))
    (description "Launch tailscaled.")))
