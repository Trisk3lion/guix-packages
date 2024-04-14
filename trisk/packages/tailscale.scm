(define-module (trisk packages tailscale)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define* (tailscale-base #:key arch hash)
  (package
    (name (string-append "tailscale-" arch "-bin"))
    (version "1.64.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_" version
                                  "_" arch ".tgz"))
              (sha256
               (base32 hash))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`((,(string-append "tailscale_" #$version "_" #$arch "/tailscaled") "sbin/")
          (,(string-append "tailscale_" #$version "_" #$arch "/tailscale") "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program
                  (string-append (assoc-ref outputs "out") "/sbin/tailscaled")
                `("PATH" ":" prefix (,(dirname (search-input-file
                                                inputs
                                                "/sbin/iptables"))
                                     ,(dirname (search-input-file
                                                inputs
                                                "/sbin/ip"))))))))))
    (inputs (list iproute iptables))
    (synopsis "Tailscale connects your team's devices and development environments for easy access to remote resources.")
    (description
     "Tailscale is a zero config VPN for building secure networks. Install on any device in minutes. Remote access from any network or physical location.")
    (home-page "https://tailscale.com/")
    ;; (supported-systems '("x86_64-linux"))
    (license #f)))


(define-public tailscale-amd64-bin
  (tailscale-base #:arch "amd64" #:hash "1kvdzgyjgp6sd2wxhq99xfsp66pb4gj7m672d477w9x2a77ds7m8"))

(define-public tailscale-arm-bin
  (tailscale-base #:arch "arm" #:hash "15ahjl45091mwjf3465lyx556wmdiws76dk3dz54a684f132a3zl"))

(define-public tailscale-arm64-bin
  (tailscale-base #:arch "arm64" #:hash "06qml88zw4m7v9aqm0pz46y6aqibhxfl4k5g2ljwsyr9s8z6h0xl"))
