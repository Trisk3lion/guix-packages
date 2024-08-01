(define-module (trisk packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check))

(define-public go-github-com-google-go-cmp-cmp-0-6
  (package
    (name "go-github-com-google-go-cmp-cmp")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n1j4hi50bl05pyys4i7y417k9g6k1blslj27z327qny7kkdl2ma"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-cmp/cmp"
       #:unpack-path "github.com/google/go-cmp"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys #:rest args)
             (unless
                 ;; The tests fail when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
               (apply (assoc-ref %standard-phases 'check) args)))))))
    (home-page "https://github.com/google/go-cmp")
    (synopsis "Package for equality of Go values")
    (description
     "This package is intended to be a more powerful and safer alternative to
@@code{reflect.@code{DeepEqual}} for comparing whether two values are
semantically equal.")
    (license license:bsd-3)))

(define-public go-golang-org-x-telemetry
  (package
    (name "go-golang-org-x-telemetry")
    (version "0.0.0-20240704191351-af73eac657e1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/telemetry")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hraq9hza6dqs0vi9rdaxjzn3v38hys514j7i3iwd4hjbyjlsj6k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.20
      #:tests? #f
      #:import-path "golang.org/x/telemetry"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-mod))
    (home-page "https://golang.org/x/telemetry")
    (synopsis "Go Telemetry")
    (description
     "This repository holds the Go Telemetry server code and libraries, used for
hosting @@url{https://telemetry.go.dev,telemetry.go.dev} and instrumenting Go
toolchain programs with opt-in telemetry.")
    (license license:bsd-3)))

(define-public go-mvdan-cc-xurls-v2
  (package
    (name "go-mvdan-cc-xurls-v2")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/xurls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1516hwlxbnhdca56qy7sx9h2n5askq6ddqpqyp3f5rvmzdkxf4zn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "mvdan.cc/xurls/v2"
      #:unpack-path "mvdan.cc/xurls/v2"))
    (propagated-inputs
     (list go-golang-org-x-sync
           go-golang-org-x-mod
           go-github-com-rogpeppe-go-internal))
    (home-page "https://mvdan.cc/xurls/v2")
    (synopsis "xurls")
    (description
     "Package xurls extracts urls from plain text using regular expressions.")
    (license license:bsd-3)))

(define-public gopls-16
  (package
    (name "gopls-16")
    (version "0.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qqcksic5hxh1v8pb08zyy078hpsinc7jp60k0zf4fmnvzwy4l58"))))
    (build-system go-build-system)
    (arguments
     `(#:go ,go-1.20
       #:import-path "golang.org/x/tools/gopls"
       #:unpack-path "golang.org/x/tools"
       #:install-source? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'unpack 'override-tools
                    (lambda _
                      (delete-file-recursively "src/golang.org/x/tools"))))))
    (propagated-inputs
     (list go-mvdan-cc-xurls-v2
           go-mvdan-cc-gofumpt
           go-honnef-co-go-tools
           go-gopkg-in-yaml-v3
           go-golang-org-x-vuln
           go-golang-org-x-tools
           go-golang-org-x-text
           go-golang-org-x-telemetry
           go-golang-org-x-sync
           go-golang-org-x-mod
           go-github-com-jba-templatecheck
           go-github-com-sergi-go-diff
           go-github-com-google-go-cmp-cmp-0-6))
    (home-page "https://golang.org/x/tools/gopls")
    (synopsis ", the Go language server")
    (description
     "Gopls (pronounced “go please”) is an LSP server for Go.  The Language Server
Protocol allows any text editor to be extended with IDE-like features; see
@@url{https://langserver.org/,https://langserver.org/} for details.")
    (license license:bsd-3)))


(define-public go-k8s-io-git-sync
  (package
    (name "go-k8s-io-git-sync")
    (version "4.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/git-sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hp8qwazv500i19rjdy1s185j1l5n03dr9xr4v4jq7m7s8xzgaaq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:install-source? #f
      #:import-path "k8s.io/git-sync"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-strange-import
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/main.go")
                (("// import \"k8s.io/git-sync/cmd/git-sync\"")
                 "")))))))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-go-uber-org-goleak
           go-github-com-spf13-pflag
           go-github-com-prometheus-client-golang
           ))
    (home-page "https://k8s.io/git-sync")
    (synopsis "git-sync")
    (description
     "git-sync is a simple command that pulls a git repository into a local directory.
 It is a perfect \"sidecar\" container in Kubernetes - it can periodically pull
files down from a repository so that an application can consume them.")
    (license license:asl2.0)))

(define-public gpodder2go
  (package
    (name "gpodder2go")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Trisk3lion/gpodder2go")
             (commit "4f14fec4e19a58851930a6969e8baf63f74cfda9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "186m0qg459kn9yki4ifw1165h61bkjl2jd7iha28zzyx0k6zm4c3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/oxtyped/gpodder2go"))
    (home-page "https://github.com/oxtyped/gpodder2go")
    (synopsis "gpodder2go")
    (description
     "gpodder2go is a simple self-hosted, golang, drop-in replacement for
gpodder/mygpo server to handle podcast subscriptions management for gpodder
clients.")
    (license license:agpl3)))
