(define-module (trisk packages tailscale)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (trisk utils go-fetch-vendored)
  #:use-module (guix licenses))


(define-public tailscale-vendored
  (let ((version "1.74.1"))
    (package
      (name "tailscale")
      (version version)
      (source (origin
                (method go-fetch-vendored)
                (uri (go-git-reference
                      (url "https://github.com/tailscale/tailscale")
                      (commit "v1.74.1")
                      (sha (base32 "0ncck013rzbrzcbpya1fq41jrgzxw22pps77l9kb7kx06as8bggb"))))
                (sha256
                 (base32
                  "19sv3q0hgb1h5v75c8hrkna4xgbgrs0ym2kvq16rbn9kr0hjjr1j"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "tailscale.com/cmd/tailscale"
         #:unpack-path "tailscale.com"
         #:install-source? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'check))
         #:go ,go-1.23))
      (home-page "https://tailscale.com")
      (synopsis "Tailscale client")
      (description "Tailscale client")
      (license license:bsd-3))))

(define-public tailscaled-vendored
  (let ((import-path "tailscale.com/cmd/tailscaled"))
    (package
      (inherit tailscale-vendored)
      (name "tailscaled")
      (arguments
       (substitute-keyword-arguments (package-arguments tailscale)
         ((#:import-path _ #f)
          import-path)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (replace 'build
                (lambda _
                  ;; idk why but we have to unset GO111MODULE in order for the build to work
                  ;; [btv] maybe vendor stuff is not getting picked up in go path?
                  (unsetenv "GO111MODULE")
                  (chdir "./src/tailscale.com")
                  (invoke "go" "build" "-o" "tailscaled"
                          #$import-path)
                  (chdir "../..")))
              (replace 'install
                (lambda _
                  (install-file "src/tailscale.com/tailscaled" (string-append #$output "/bin"))))))))
      (synopsis "Tailscale daemon")
      (description "Tailscale daemon"))))

(define* (tailscale-base #:key arch hash)
  (package
    (name (string-append "tailscale-" arch "-bin"))
    (version "1.64.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                           version "_" arch ".tgz"))
       (sha256
        (base32 hash))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~`((,(string-append "tailscale_"
                                          #$version "_"
                                          #$arch "/tailscaled") "sbin/")
                         (,(string-append "tailscale_"
                                          #$version "_"
                                          #$arch "/tailscale") "bin/"))
      ;; #:phases #~(modify-phases %standard-phases
      ;;              (add-after 'install 'wrap-binary
      ;;                (lambda* (#:key inputs outputs #:allow-other-keys)
      ;;                  (wrap-program (string-append (assoc-ref outputs "out")
      ;;                                               "/sbin/tailscaled")
      ;;                    `("PATH" ":" prefix
      ;;                      (,(dirname (search-input-file inputs
      ;;                                                    "/sbin/iptables")) ,(dirname
      ;;                                                                         (search-input-file
      ;;                                                                          inputs
      ;;                                                                          "/sbin/ip"))))))))
      ))
    ;; (inputs (list iproute iptables))
    (synopsis
     "Tailscale connects your team's devices and development environments for easy access to remote resources.")
    (description
     "Tailscale is a zero config VPN for building secure networks. Install on any device in minutes. Remote access from any network or physical location.")
    (home-page "https://tailscale.com/")
    ;; (supported-systems '("x86_64-linux"))
    (license #f)))

(define-public tailscale-amd64-bin
  (tailscale-base #:arch "amd64"
                  #:hash
                  "1kvdzgyjgp6sd2wxhq99xfsp66pb4gj7m672d477w9x2a77ds7m8"))

(define-public tailscale-arm-bin
  (tailscale-base #:arch "arm"
                  #:hash
                  "15ahjl45091mwjf3465lyx556wmdiws76dk3dz54a684f132a3zl"))

(define-public tailscale-arm64-bin
  (tailscale-base #:arch "arm64"
                  #:hash
                  "06qml88zw4m7v9aqm0pz46y6aqibhxfl4k5g2ljwsyr9s8z6h0xl"))
