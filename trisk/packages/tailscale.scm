(define-module (trisk packages tailscale)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (trisk utils go-fetch-vendored)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tailscale-vendored
  (let ((version "1.74.1"))
    (package
      (name "tailscale-vendored")
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
      (name "tailscaled-vendored")
      (arguments
       (substitute-keyword-arguments (package-arguments tailscale-vendored)
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


(define-public tailscale-amd64-bin
  (package
    (name "tailscale-amd64-bin")
    (version "1.76.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_amd64.tgz"))
              (sha256
               (base32 "0dk0p4jc91p6c1jg944ljvanj85r3szjs6zl4xh71anq20vlj4bb"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:install-plan
       #~'(("tailscaled" "sbin/")
           ("tailscale" "bin/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'wrap-binary
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program
                 (string-append (assoc-ref outputs "out") "/sbin/tailscaled")
                 `("PATH" ":" prefix (,(dirname (search-input-file
                                                  inputs "/sbin/iptables"))
                                      ,(dirname (search-input-file
                                                  inputs "/sbin/ip")))))))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out #$output)
                      (tailscale (string-append out "/bin/tailscale"))
                      (share (string-append out "/share"))
                      (bash (string-append out "/etc/bash_completion.d/tailscale"))
                      )
                 (mkdir-p (dirname bash))
                 (with-output-to-file bash
                   (lambda ()
                     (invoke tailscale "completion" "bash")))))))))
    (inputs (list iproute iptables))
    (home-page "https://github.com/tailscale/tailscale")
    (synopsis "Tailscale VPN client")
    (description "Tailscale lets you easily manage access to private resources,
quickly SSH into devices on your network, and work securely from anywhere in
the world.")
    ;(properties
    ; `(;(hidden? . #t)
    ;   (release-monitoring-url . "https://github.com/tailscale/tailscale/releases")
    ;   (upstream-name . "tailscale")))
    (supported-systems '("x86_64-linux"))
    (license license:bsd-3)))

(define-public tailscale-386-bin
  (package
    (inherit tailscale-amd64-bin)
    (name "tailscale-bin-386")
    (version "1.76.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_386.tgz"))
              (sha256
               (base32 "0ihnmha6vg8a2bss7dxyp78c1wrcpdz2hv87hsph2spk7fvn8167"))))
    (supported-systems '("i686-linux"))))

(define-public tailscale-arm-bin
  (package
    (inherit tailscale-amd64-bin)
    (name "tailscale-bin-arm")
    (version "1.76.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm.tgz"))
              (sha256
               (base32 "10asp7r1h6ipsb7gn4570v2ydal137hdachpygvha5jqkzhxj40m"))))
    (supported-systems '("armhf-linux"))))

(define-public tailscale-arm64-bin
  (package
    (inherit tailscale-amd64-bin)
    (name "tailscale-bin-arm64")
    (version "1.76.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm64.tgz"))
              (sha256
               (base32 "06036495pymqyv0ppsnw512c5f2k21s9n7fvjqfm2mcswkrn0drz"))))
    (supported-systems '("aarch64-linux"))))
