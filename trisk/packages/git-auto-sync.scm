(define-module (trisk packages git-auto-sync)
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
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages version-control))

(define-public go-github-com-godbus-dbus-v5
  (package
    (name "go-github-com-godbus-dbus-v5")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/godbus/dbus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kayd4x7idrhi06ahh5kqkgwzgh9icvv71mjar2d0jl486dfs8r5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/godbus/dbus/v5"))
    (home-page "https://github.com/godbus/dbus")
    (synopsis "dbus")
    (description
     "Package dbus implements bindings to the D-Bus message bus system.")
    (license license:bsd-2)))

(define-public go-github-com-gen2brain-beeep
  (package
    (name "go-github-com-gen2brain-beeep")
    (version "0.0.0-20240516210008-9c006672e7f4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gen2brain/beeep")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1izdc8227b9irpdzv7kcyahcgna0qf82nzva0c6043lnfzaf03l2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file "notify_windows.go")))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/gen2brain/beeep"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/gen2brain/beeep")
    (synopsis "beeep")
    (description
     "Package beeep provides a cross-platform library for sending desktop
notifications and beeps.")
    (license license:bsd-2)))

(define-public go-github-com-kardianos-service
  (package
    (name "go-github-com-kardianos-service")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kardianos/service")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wcrc632nf2l5gzwgjfpx7bh6v4la0qjmaykj58fysabb9fkk9dy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/kardianos/service"))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/kardianos/service")
    (synopsis "service")
    (description
     "Package service provides a simple way to create a system service.  Currently
supports Windows, Linux/(systemd | Upstart | @code{SysV} | @code{OpenRC}), and
OSX/Launchd.")
    (license license:zlib)))

(define-public go-github-com-kirsle-configdir
  (package
    (name "go-github-com-kirsle-configdir")
    (version "0.0.0-20170128060238-e45d2f54772f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kirsle/configdir")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hlidrhlsrkazy9l6wjq2ylr4rwswm7p5f0is68s18g8rcvamgn3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kirsle/configdir"))
    (home-page "https://github.com/kirsle/configdir")
    (synopsis "ConfigDir for Go")
    (description
     "Package configdir provides a cross platform means of detecting the system's
configuration directories.")
    (license license:expat)))

(define-public go-github-com-rjeczalik-notify
  (package
    (name "go-github-com-rjeczalik-notify")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rjeczalik/notify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yl4yqx3qxi2adl6hpg4kzx6crhaksr237wnzhqmj89gqvvwgxmr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/rjeczalik/notify"))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/rjeczalik/notify")
    (synopsis "notify")
    (description "Package notify implements access to filesystem events.")
    (license license:expat)))

(define-public go-github-com-ztrue-tracerr
  (package
    (name "go-github-com-ztrue-tracerr")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ztrue/tracerr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05sqh7aka7lrpldy5h5v1wmai0b5621p1cj9kk7k77pc02g25d7h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ztrue/tracerr"))
    (home-page "https://github.com/ztrue/tracerr")
    (synopsis "Golang Errors with Stack Trace and Source Fragments")
    (description
     "Package tracerr makes error output more informative.  It adds stack trace to
error and can display error with source fragments.")
    (license license:expat)))

(define-public go-github-com-armon-go-socks5
  (package
    (name "go-github-com-armon-go-socks5")
    (version "0.0.0-20160902184237-e75332964ef5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/go-socks5")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104w10jf0wlxyxi35hf6frndgf0ybz21h54xjmnkivpb6slycpyq"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* '("socks5.go"
                                  "ruleset.go"
                                  "ruleset_test.go"
                                  "resolver.go"
                                  "resolver_test.go"
                                  "request.go")
                     (("golang.org/x/net/context")
                      "context"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/armon/go-socks5"))
    (propagated-inputs
     (list go))
    (home-page "https://github.com/armon/go-socks5")
    (synopsis "go-socks5")
    (description
     "This package provides the @@code{socks5} package that implements a
@@url{http://en.wikipedia.org/wiki/SOCKS,SOCKS5 server}.  SOCKS (Secure Sockets)
is used to route traffic between a client and server through an intermediate
proxy layer.  This can be used to bypass firewalls or NATs.")
    (license license:expat)))

(define-public go-github-com-gliderlabs-ssh
  (package
    (name "go-github-com-gliderlabs-ssh")
    (version "0.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gliderlabs/ssh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162r7i4lhdbxa6m5i2n1fw444ka2q8xx26l491qqik8s5cwmyzbx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gliderlabs/ssh"))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-github-com-anmitsu-go-shlex))
    (home-page "https://github.com/gliderlabs/ssh")
    (synopsis "gliderlabs/ssh")
    (description
     "Package ssh wraps the crypto/ssh package with a higher-level API for building
SSH servers.  The goal of the API was to make it as simple as using net/http, so
the API is very similar.")
    (license license:bsd-3)))

(define-public go-gopkg-in-warnings-v0
  (package
    (name "go-gopkg-in-warnings-v0")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/warnings.v0")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kzj50jn708cingn7a13c2wdlzs6qv89dr2h4zj8d09647vlnd81"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/warnings.v0"
      #:unpack-path "gopkg.in/warnings.v0"))
    (home-page "https://gopkg.in/warnings.v0")
    (synopsis #f)
    (description
     "Package warnings implements error handling with non-fatal errors (warnings).")
    (license license:bsd-2)))

(define-public go-github-com-src-d-gcfg
  (package
    (name "go-github-com-src-d-gcfg")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/src-d/gcfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "044j95skmyrwjw5fwjk6ka32rjgsg0ar0mfp9np19sh1acwv4x4r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/src-d/gcfg"))
    (propagated-inputs
     (list go-gopkg-in-warnings-v0))
    (home-page "https://github.com/src-d/gcfg")
    (synopsis #f)
    (description
     "Package gcfg reads \"INI-style\" text-based configuration files with \"name=value\"
pairs grouped into sections (gcfg files).")
    (license license:bsd-3)))

(define-public go-gopkg-in-src-d-go-billy-v4
  (package
    (name "go-gopkg-in-src-d-go-billy-v4")
    (version "4.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/src-d/go-billy.v4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jcyi4ink2njgflp3f2mbl5b86p2w0rh945k5xplcl158i5wkrc6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/src-d/go-billy.v4"
      #:unpack-path "gopkg.in/src-d/go-billy.v4"))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-golang-org-x-sys))
    (home-page "https://gopkg.in/src-d/go-billy.v4")
    (synopsis "go-billy")
    (description
     "The missing interface filesystem abstraction for Go.  Billy implements an
interface based on the @@code{os} standard library, allowing to develop
applications without dependency on the underlying storage.  Makes it virtually
free to implement mocks and testing over filesystem operations.")
    (license license:asl2.0)))

(define-public go-gopkg-in-src-d-go-git-v4
  (package
    (name "go-gopkg-in-src-d-go-git-v4")
    (version "4.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/src-d/go-git.v4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n4x7r69qrmpss51b3wd3vj4b9jmip4archz3fbqk6q1yyd1pxjb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "gopkg.in/src-d/go-git.v4"
      #:unpack-path "gopkg.in/src-d/go-git.v4"))
    (propagated-inputs
     (list go-github-com-go-git-go-git-fixtures
           go-gopkg-in-src-d-go-billy-v4
           go-gopkg-in-check-v1
           go-golang-org-x-text
           go-golang-org-x-net
           go-golang-org-x-crypto
           go-github-com-xanzy-ssh-agent
           go-github-com-src-d-gcfg
           go-github-com-sergi-go-diff
           go-github-com-mitchellh-go-homedir
           go-github-com-kevinburke-ssh-config
           go-github-com-jessevdk-go-flags
           go-github-com-jbenet-go-context
           go-github-com-google-go-cmp
           go-github-com-gliderlabs-ssh
           go-github-com-emirpasic-gods
           go-github-com-armon-go-socks5))
    (home-page "https://gopkg.in/src-d/go-git.v4")
    (synopsis "Comparison with git")
    (description
     "This package provides a highly extensible git implementation in pure Go.")
    (license license:asl2.0)))

(define-public go-gotest-tools-v3
  (package
    (name "go-gotest-tools-v3")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gotestyourself/gotest.tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r5mc6slab6fj2si9nripl7fdq097s694gsn1gsxg2wj7605m5v4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gotest.tools/v3"
      #:unpack-path "gotest.tools/v3"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-google-go-cmp))
    (home-page "https://gotest.tools/v3")
    (synopsis "gotest.tools")
    (description
     "Package gotesttools is a collection of packages to augment `testing` and support
common patterns.")
    (license license:asl2.0)))

(define-public go-github-com-gitjournal-git-auto-sync
  (package
    (name "go-github-com-gitjournal-git-auto-sync")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GitJournal/git-auto-sync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00crvq2naggma9z83wxfnwmr58l20fc9qpv9l9sjgxrqf07jd5r9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/GitJournal/git-auto-sync"))
    (propagated-inputs
     (list go-gotest-tools-v3
           go-gopkg-in-src-d-go-git-v4
           go-github-com-ztrue-tracerr
           go-github-com-urfave-cli-v2
           go-github-com-rjeczalik-notify
           go-github-com-otiai10-copy
           go-github-com-kirsle-configdir
           go-github-com-kardianos-service
           go-github-com-gen2brain-beeep))
    (home-page "https://github.com/GitJournal/git-auto-sync")
    (synopsis "Git Auto Sync")
    (description
     "Lets write this in @code{GoLang} as that's the easiest to do.  It'll also work
across all platforms, and it will be simple to deploy.  Very low memory
footprint.")
    (license license:asl2.0)))
