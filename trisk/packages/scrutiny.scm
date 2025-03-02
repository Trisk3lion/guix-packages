(define-module (trisk packages scrutiny)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages prometheus))

(define-public go-github-com-kvz-logstreamer
  (package
    (name "go-github-com-kvz-logstreamer")
    (version "0.0.0-20221024075423-bf5cfbd32e39")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kvz/logstreamer")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x9prmh6m6nfi6hmiczs6k85pvqni00961gnip8b17klpw2ii8ng"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kvz/logstreamer"))
    (home-page "https://github.com/kvz/logstreamer")
    (synopsis "logstreamer")
    (description "Prefixes streams (e.g. stdout or stderr) in Go.")
    (license license:expat)))

(define-public go-github-com-analogj-go-util
  (package
    (name "go-github-com-analogj-go-util")
    (version "0.0.0-20210417161720-39b497cca03b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AnalogJ/go-util")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pi1nyhyrcgpy9dpkvpzn8k81cdg56y3mkfdbwf5y575xvk1d1wk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/analogj/go-util/utils"
      #:unpack-path "github.com/analogj/go-util"))
    (propagated-inputs (list go-gopkg-in-yaml-v2
                             go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-mitchellh-go-homedir
                             go-github-com-kvz-logstreamer
                             go-github-com-fatih-color))
    (home-page "https://github.com/analogj/go-util")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-maxatome-go-testdeep
  (package
    (name "go-github-com-maxatome-go-testdeep")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maxatome/go-testdeep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r9w79qm6j080gbqhrd5gwjzsnkmrcihy4yniw77g0wkspxxdjww"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/maxatome/go-testdeep"))
    (propagated-inputs (list go-github-com-davecgh-go-spew))
    (home-page "https://github.com/maxatome/go-testdeep")
    (synopsis "go-testdeep")
    (description
     "Package testdeep allows extremely flexible deep comparison.  It is built for
testing.")
    (license license:bsd-2)))

(define-public go-github-com-jarcoal-httpmock
  (package
    (name "go-github-com-jarcoal-httpmock")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jarcoal/httpmock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xw73d59nl1jj18h2hp9vlgqmfvqk9bknzpimg4mjn13d4jzhqrf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jarcoal/httpmock"))
    (propagated-inputs (list go-github-com-maxatome-go-testdeep))
    (home-page "https://github.com/jarcoal/httpmock")
    (synopsis "httpmock")
    (description "Package httpmock provides tools for mocking HTTP responses.")
    (license license:expat)))

(define-public go-github-com-containrrr-shoutrrr
  (package
    (name "go-github-com-containrrr-shoutrrr")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containrrr/shoutrrr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bjswm2p0273dk2sl1a051azdqlc2y88famcsmmpyrqidaiqav0c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containrrr/shoutrrr"))
    (propagated-inputs (list go-golang-org-x-net
                             go-golang-org-x-oauth2
                             go-github-com-spf13-viper
                             go-github-com-spf13-cobra
                             go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo-v2
                             go-github-com-mattn-go-isatty
                             go-github-com-mattn-go-colorable
                             go-github-com-jarcoal-httpmock
                             go-github-com-fatih-color))
    (home-page "https://github.com/containrrr/shoutrrr")
    (synopsis "Shoutrrr")
    (description
     "Using shoutrrr is easy! There is currently two ways of using it as a package.")
    (license license:expat)))

(define-public go-github-com-glebarez-go-sqlite
  (package
    (name "go-github-com-glebarez-go-sqlite")
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glebarez/go-sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11bnglrk8iwjlsgmw0cy4sy1k408qq6hwnsyl0gslwsdi9jvrngi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/glebarez/go-sqlite"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-strange-import
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (map (lambda (file)
                                  (string-append "src/" import-path "/" file))
                                '("sqlite.go" "mutex.go" "rlimit.go" "norlimit.go" "sqlite_go18.go" "rulimit.go" "sqlite_go18_test.go" "all_test.go"))
                (("// import \"modernc.org/sqlite\"")
                 "")))))))
    (propagated-inputs (list go-modernc-org-sqlite
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-golang-org-x-sys
                             go-github-com-google-pprof))
    (home-page "https://github.com/glebarez/go-sqlite")
    (synopsis "go-sqlite")
    (description
     "This is a pure-Go SQLite driver for Golang's native
@@url{https://pkg.go.dev/database/sql,database/sql} package.  The driver has
@@url{https://gitlab.com/cznic/sqlite,Go-based implementation of SQLite}
embedded in itself (so, you don't need to install SQLite separately).")
    (license license:bsd-3)))

(define-public go-modernc-org-gc-v3
  (package
    (name "go-modernc-org-gc")
    (version "3.0.0-20241223112719-96e2e1e4408d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/gc")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hd7mi73f241s13sw1853vw5v0d5imd8i31ygws8sjgnfa599a5f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "modernc.org/gc/v3"
      #:unpack-path "modernc.org/gc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-mathutil
                             go-golang-org-x-tools
                             go-golang-org-x-exp
                             go-github-com-pmezard-go-difflib
                             go-github-com-hashicorp-golang-lru-v2
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/gc")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-ncruces-go-strftime
  (package
    (name "go-github-com-ncruces-go-strftime")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncruces/go-strftime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rmr44m8mj5w9j1sy4c24b3n55lx2gwz1z3lb2g3p4qw87wv0j2g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ncruces/go-strftime"))
    (home-page "https://github.com/ncruces/go-strftime")
    (synopsis "/")
    (description
     "Package strftime provides strftime/strptime compatible time formatting and
parsing.")
    (license license:expat)))

(define-public go-modernc-org-sortutil
  (package
    (name "go-modernc-org-sortutil")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sortutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01s9vil8lvaz526x6q9f12h6vpc3jc8zvpag7knz1bdx9b15yljc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/sortutil"))
    (propagated-inputs (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/sortutil")
    (synopsis #f)
    (description
     "Package sortutil provides utilities supplementing the standard sort package.")
    (license license:bsd-3)))

(define-public go-modernc-org-cc-v4
  (package
    (name "go-modernc-org-cc")
    (version "4.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18kxly8vlz0ywfb4m9bnkdbc07fvp6h3x6588jmvd5cik74c4j8l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "modernc.org/cc/v4"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-sortutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-ccorpus2
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis #f)
    (description "Package cc is a C99 compiler front end.")
    (license license:bsd-3)))

(define-public go-lukechampine-com-uint128
  (package
    (name "go-lukechampine-com-uint128")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lukechampine/uint128")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yy6lbprrsl9gscxn4194kr5sfvgi9cgvjdxn2141k36ab3nz8ip"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "lukechampine.com/uint128"))
    (home-page "https://lukechampine.com/uint128")
    (synopsis "uint128")
    (description
     "@@code{uint128} provides a high-performance @@code{Uint128} type that supports
standard arithmetic operations.  Unlike @@code{math/big}, operations on
@@code{Uint128} values always produce new values instead of modifying a pointer
receiver.  A @@code{Uint128} value is therefore immutable, just like
@@code{uint64} and friends.")
    (license license:expat)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "3.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khw9qsaz4ab0vb4kazgfm481cjpcyxj6ld2ma4d9hva3ca9h8ji"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v3"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-mathutil
                             go-lukechampine-com-uint128
                             go-github-com-google-go-cmp
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-httpfs
  (package
    (name "go-modernc-org-httpfs")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/httpfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01q5rvhxmrd45h0ljh4185wlly7rxv6vvh28d2shsyan4nj67zf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/httpfs"))
    (home-page "https://modernc.org/httpfs")
    (synopsis "httpfs")
    (description
     "Package httpfs implements http.@code{FileSystem} on top of a map[string]string.")
    (license license:bsd-3)))

(define-public go-modernc-org-ccorpus
  (package
    (name "go-modernc-org-ccorpus")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccorpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18d5npw8aw5qzy6qcrlrili2zxvmc2v4kkwjps6c3ayvi7aj7j09"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccorpus"))
    (propagated-inputs (list go-modernc-org-httpfs))
    (home-page "https://modernc.org/ccorpus")
    (synopsis "ccorpus")
    (description "Package ccorpus provides a test corpus of C code.")
    (license license:bsd-3)))

;; (define-public go-modernc-org-ccgo-v3
;;   (package
;;     (name "go-modernc-org-ccgo")
;;     (version "3.17.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://gitlab.com/cznic/ccgo")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0zkxzhmkm5pgalcnlhw0whiii6vhdpnnnpwkx9b5ah21ajk6qqlc"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:import-path "modernc.org/ccgo/v3"
;;       #:unpack-path "modernc.org/ccgo"))
;;     (propagated-inputs (list go-modernc-org-opt
;;                              go-modernc-org-mathutil
;;                              go-modernc-org-libc
;;                              go-modernc-org-ccorpus
;;                              go-modernc-org-ccgo-v4
;;                              go-modernc-org-cc-v3
;;                              go-golang-org-x-tools
;;                              go-golang-org-x-sys
;;                              go-github-com-pmezard-go-difflib
;;                              go-github-com-kballard-go-shellquote
;;                              go-github-com-dustin-go-humanize))
;;     (home-page "https://modernc.org/ccgo")
;;     (synopsis "ccgo/v3")
;;     (description "Package ccgo translates C to Go source code.")
;;     (license license:bsd-3)))

(define-public go-modernc-org-ccorpus2
  (package
    (name "go-modernc-org-ccorpus2")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccorpus2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mwgi0jdj5a595wlllr5rn3gvl7cqz7fnjx28hn3ia9cs1nxkk0a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccorpus2"))
    (home-page "https://modernc.org/ccorpus2")
    (synopsis "ccorpus2")
    (description "Package ccorpus2 provides a test corpus of C code.")
    (license license:bsd-3)))

(define-public go-modernc-org-lex
  (package
    (name "go-modernc-org-lex")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fad093cdkgdwk3sf0vklk05qzkis1ivri3hig1wigv4z908nmdj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lex"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-fileutil))
    (home-page "https://modernc.org/lex")
    (synopsis #f)
    (description
     "Package lex provides support for a *nix (f)lex like tool on .l sources.  The
syntax is similar to a subset of (f)lex, see also:
@@url{http://flex.sourceforge.net/manual/Format.html#Format,http://flex.sourceforge.net/manual/Format.html#Format}.")
    (license license:bsd-3)))

(define-public go-modernc-org-lexer
  (package
    (name "go-modernc-org-lexer")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lexer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ahagg81vbm8k9an5l8gn8plr7d53955v54mv0g811hfqxacja2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lexer"))
    (propagated-inputs (list go-modernc-org-fileutil go-golang-org-x-exp))
    (home-page "https://modernc.org/lexer")
    (synopsis #f)
    (description
     "Package lexer provides generating actionless scanners (lexeme recognizers) at
run time.")
    (license license:bsd-3)))

(define-public go-modernc-org-scannertest
  (package
    (name "go-modernc-org-scannertest")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/scannertest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hk8pqaihhmfxfprg1fmdl2y8ffvrblm10z7qq3l921jjxc1ch7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/scannertest"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-lex))
    (home-page "https://modernc.org/scannertest")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-modernc-org-token
  (package
    (name "go-modernc-org-token")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/token")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vvnnfppmgq7hxmw18dx90fg6khwnxpwn9kwwf0hwxsckxfb5icv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/token"))
    (home-page "https://modernc.org/token")
    (synopsis "token")
    (description
     "Package token is variant of the stdlib package token with types @code{FileSet}
and Token removed.")
    (license license:bsd-3)))

(define-public go-modernc-org-gc-v2
  (package
    (name "go-modernc-org-gc")
    (version "2.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/gc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hd7mi73f241s13sw1853vw5v0d5imd8i31ygws8sjgnfa599a5f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/gc/v2"
      #:unpack-path "modernc.org/gc"))
    (propagated-inputs (list go-modernc-org-token go-modernc-org-scannertest
                             go-github-com-pmezard-go-difflib
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/gc")
    (synopsis "gc")
    (description
     "Package GC is a Go compiler front end. (Work in progress, API unstable).")
    (license license:bsd-3)))

(define-public go-modernc-org-opt
  (package
    (name "go-modernc-org-opt")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/opt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rfh7qwg2j9kd7sgxs8nx6vbpikz5w1y7dpsv8hrkv74vp9v1854"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/opt"))
    (home-page "https://modernc.org/opt")
    (synopsis "opt")
    (description "Package opt implements command-line flag parsing.")
    (license license:bsd-3)))

(define-public go-modernc-org-strutil
  (package
    (name "go-modernc-org-strutil")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/strutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rk76c1n189hzg3kfab8pfvssa1h9v0vxk5jxy8pk32rqic0hdim"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/strutil"))
    (propagated-inputs (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/strutil")
    (synopsis #f)
    (description
     "Package strutil collects utils supplemental to the standard strings package.")
    (license license:bsd-3)))

;; (define-public go-modernc-org-ccgo-v4
;;   (package
;;     (name "go-modernc-org-ccgo")
;;     (version "4.23.5")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://gitlab.com/cznic/ccgo")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1qm8y8wdl9wwrrlasns10idjn0pplzifnvv9x82xhbv3y90nrg6q"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:tests? #f
;;       #:import-path "modernc.org/ccgo"))
;;     (propagated-inputs (list go-modernc-org-strutil
;;                              go-modernc-org-opt
;;                              go-modernc-org-mathutil
;;                              go-modernc-org-libc
;;                              go-modernc-org-gc-v2
;;                              go-modernc-org-fileutil
;;                              go-modernc-org-ccorpus2
;;                              go-modernc-org-ccgo-v3
;;                              go-modernc-org-cc-v4
;;                              go-golang-org-x-tools
;;                              go-golang-org-x-mod
;;                              go-github-com-pmezard-go-difflib
;;                              go-github-com-pbnjay-memory
;;                              go-github-com-dustin-go-humanize))
;;     (home-page "https://modernc.org/ccgo")
;;     (synopsis #f)
;;     (description "Command ccgo is a C compiler producing Go code.")
;;     (license license:bsd-3)))

(define-public go-modernc-org-fileutil
  (package
    (name "go-modernc-org-fileutil")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/fileutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10q5xbik9yk9jw2ziq1fw0hpjfbv5h3qm7rlxlkwj0qxyyb7b9bi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/fileutil"))
    (propagated-inputs (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/fileutil")
    (synopsis #f)
    (description "Package fileutil collects some file utility functions.")
    (license license:bsd-3)))

(define-public go-modernc-org-memory
  (package
    (name "go-modernc-org-memory")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/memory")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066pzk0i1jxahialzp97ra0k0f191y1756sgppiw50zkpnpwzjxr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/memory"))
    (propagated-inputs (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/memory")
    (synopsis "memory")
    (description "Package memory implements a memory allocator.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.61.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/libc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lil3638vkjadcv8cq253zi2401rs661cpfx7jf50hx82ccy97kf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "modernc.org/libc"))
    (propagated-inputs (list go-modernc-org-memory
                             go-modernc-org-mathutil
                             go-modernc-org-fileutil
                             go-modernc-org-cc-v4
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-github-com-ncruces-go-strftime
                             go-github-com-mattn-go-isatty
                             go-github-com-google-uuid
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
     "Package libc provides run time support for programs generated by the
@@url{http://modernc.org/ccgo/v4,ccgo} C to Go transpiler, version 4 or later.")
    (license license:bsd-3)))

(define-public go-github-com-remyoudompheng-bigfft
  (package
    (name "go-github-com-remyoudompheng-bigfft")
    (version "0.0.0-20230129092748-24d4a6f8daec")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/remyoudompheng/bigfft")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qxfda0jq70ank99zlgfz7iig2jpicbbxnpr7xcf1v9p474ak2dx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/remyoudompheng/bigfft"))
    (home-page "https://github.com/remyoudompheng/bigfft")
    (synopsis #f)
    (description
     "Package bigfft implements multiplication of big.Int using FFT.")
    (license license:bsd-3)))

(define-public go-modernc-org-mathutil
  (package
    (name "go-modernc-org-mathutil")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/mathutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09yhqhyaq5ikqm8afj09vr1v1ji7lwvd16mysr8hb1c6bnn7krh8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "modernc.org/mathutil"))
    (propagated-inputs (list go-github-com-remyoudompheng-bigfft))
    (home-page "https://modernc.org/mathutil")
    (synopsis #f)
    (description
     "Package mathutil provides utilities supplementing the standard math and
math/rand packages.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.34.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "085fqhk2cbr7jx7hjhyhc84fwb24bj0zjnmr21qckp1dviz011v4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "modernc.org/sqlite"))
    (propagated-inputs (list go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-gc-v3
                             go-modernc-org-fileutil
                             go-golang-org-x-sys
                             go-github-com-google-pprof))
    (home-page "https://modernc.org/sqlite")
    (synopsis #f)
    (description
     "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-github-com-glebarez-sqlite
  (package
    (name "go-github-com-glebarez-sqlite")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glebarez/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bxvwps50aglzvh53fv4gnv22l7whr3p4c5g8bplpja5a7ln43c5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/glebarez/sqlite"))
    (propagated-inputs (list go-modernc-org-sqlite
                             go-gorm-io-gorm
                             go-github-com-glebarez-go-sqlite))
    (home-page "https://github.com/glebarez/sqlite")
    (synopsis "Pure-Go SQLite driver for GORM")
    (description
     "Pure-go (without cgo) implementation of SQLite driver for
@@url{https://gorm.io/,GORM} This driver has SQLite embedded, you don't need to
install one separately.")
    (license license:expat)))

(define-public go-github-com-go-gormigrate-gormigrate-v2
  (package
    (name "go-github-com-go-gormigrate-gormigrate")
    (version "2.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gormigrate/gormigrate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iljk7233rzzafxqhks4vbgks7lg68rvip5y8bwykpg8yh0aw8nn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/go-gormigrate/gormigrate"))
    (propagated-inputs (list go-gorm-io-gorm))
    (home-page "https://github.com/go-gormigrate/gormigrate")
    (synopsis "Gormigrate")
    (description
     "Package gormigrate is a minimalistic migration helper for Gorm
(@@url{http://gorm.io,http://gorm.io}).")
    (license license:expat)))

(define-public go-github-com-influxdata-line-protocol
  (package
    (name "go-github-com-influxdata-line-protocol")
    (version "0.0.0-20210922203350-b1ad95c89adf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/influxdata/line-protocol")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "001bprw364wp1rp81kd4kcd9289h5xr7byhs6989dmkk1mpvnn6g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/influxdata/line-protocol"))
    (home-page "https://github.com/influxdata/line-protocol")
    (synopsis "line-protocol")
    (description
     "This is an encoder for the influx
@@url{https://docs.influxdata.com/influxdb/latest/reference/syntax/line-protocol/,line
protocol.}.")
    (license license:expat)))

(define-public go-github-com-ravenox-go-jsoncommentstrip
  (package
    (name "go-github-com-ravenox-go-jsoncommentstrip")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RaveNoX/go-jsoncommentstrip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0igb0mxccw6q7c8m96v8wjqlcfkrpv9ac1jk4xp9i466hiphjgbc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/RaveNoX/go-jsoncommentstrip"))
    (home-page "https://github.com/RaveNoX/go-jsoncommentstrip")
    (synopsis "go-jsoncommentstrip")
    (description
     "Package jsoncommentstrip contains functions which strips comments from JSON
input.")
    (license license:expat)))

(define-public go-github-com-juju-gnuflag
  (package
    (name "go-github-com-juju-gnuflag")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/juju/gnuflag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rky87fv1nbmfk12c1m478gvl3xi8jx6d353xay4zlj286mqdbhg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/juju/gnuflag"))
    (home-page "https://github.com/juju/gnuflag")
    (synopsis "Gnuflag")
    (description
     "Package flag implements command-line flag parsing in the GNU style.  It is
almost exactly the same as the standard flag package, the only difference being
the extra argument to Parse.")
    (license license:bsd-3)))

(define-public go-github-com-spkg-bom
  (package
    (name "go-github-com-spkg-bom")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spkg/bom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm2c84mlbcaschl7db4sfydqn9kbj1a6v51adgy8m2rrhzzbrxq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spkg/bom"))
    (home-page "https://github.com/spkg/bom")
    (synopsis "bom")
    (description "Package bom is used to clean up UTF-8 Byte Order Marks.")
    (license license:expat)))

(define-public go-github-com-apapsch-go-jsonmerge-v2
  (package
    (name "go-github-com-apapsch-go-jsonmerge")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apapsch/go-jsonmerge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ca5bprbws9y31wv5x6nlzvfwg8wss5chgjm8ri6vsnw26rsc3k3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/apapsch/go-jsonmerge"
      #:unpack-path "github.com/apapsch/go-jsonmerge"))
    (propagated-inputs (list go-github-com-spkg-bom go-github-com-juju-gnuflag
                             go-github-com-bmatcuk-doublestar
                             go-github-com-ravenox-go-jsoncommentstrip))
    (home-page "https://github.com/apapsch/go-jsonmerge")
    (synopsis "go-jsonmerge")
    (description "Package jsonmerge helps mergeing JSON objects.")
    (license license:expat)))

(define-public go-github-com-knz-go-libedit
  (package
    (name "go-github-com-knz-go-libedit")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knz/go-libedit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04a5ryzldsk7agybcz4rpd7g1v5vh7smawlky58bwj0341083p44"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knz/go-libedit"))
    (home-page "https://github.com/knz/go-libedit")
    (synopsis "go-libedit")
    (description
     "Go wrapper around @@code{libedit}, a replacement to GNU readline using the BSD
license.")
    (license license:asl2.0)))

(define-public go-nullprogram-com-x-optparse
  (package
    (name "go-nullprogram-com-x-optparse")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeeto/optparse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yzpzlhmxdm8gd8ikh9c91qmshjp1jg49l0qsslmm432wk19zym9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "nullprogram.com/x/optparse"))
    (home-page "https://nullprogram.com/x/optparse")
    (synopsis "Traditional long option parser for Go")
    (description
     "Package optparse parses command line arguments very similarly to GNU
@code{getopt_long()}.  It supports long options and optional arguments, but does
not permute arguments.  It is intended as a replacement for Go's flag package.")
    (license license:unlicense)))

(define-public go-github-com-cloudwego-iasm
  (package
    (name "go-github-com-cloudwego-iasm")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/iasm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9jvx6ijlr2xz3am4qrz5py68xpl8np7m7yfq9m2ilkli3ksq9x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/iasm"))
    (propagated-inputs (list go-nullprogram-com-x-optparse
                             go-github-com-stretchr-testify
                             go-github-com-knz-go-libedit
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew))
    (home-page "https://github.com/cloudwego/iasm")
    (synopsis "-- Interactive Assembler for Go.")
    (description "Dual-purpose assembly engine written in pure Golang.")
    (license license:asl2.0)))

(define-public go-github-com-bytedance-sonic-loader
  (package
    (name "go-github-com-bytedance-sonic-loader")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytedance/sonic")
             (commit (go-version->git-ref version
                                          #:subdir "loader"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fyjq3hr4cmai2r06ppzil314bcqz416gd1zpw7lfp9h7mcwxwa4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytedance/sonic/loader"
      #:unpack-path "github.com/bytedance/sonic"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-cloudwego-iasm))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-cloudwego-base64x
  (package
    (name "go-github-com-cloudwego-base64x")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/base64x")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgs28mj5w350vp6pazz2265hx2kab3kbjw7vnk0w1skslxbj8kx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/base64x"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew
                             go-github-com-bytedance-sonic-loader))
    (home-page "https://github.com/cloudwego/base64x")
    (synopsis "base64x")
    (description
     "High performance drop-in replacement of the @@code{encoding/base64} library.")
    (license (list license:asl2.0 license:asl2.0))))

(define-public go-github-com-twitchyliquid64-golang-asm
  (package
    (name "go-github-com-twitchyliquid64-golang-asm")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twitchyliquid64/golang-asm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1akw41i0snxqw9lqzmnn4gx6hd5js5dr1vmfkm49wxans4k14vw4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twitchyliquid64/golang-asm"))
    (home-page "https://github.com/twitchyliquid64/golang-asm")
    (synopsis "golang-asm")
    (description
     "This package provides a mirror of the assembler from the Go compiler, with
import paths re-written for the assembler to be functional as a standalone
library.")
    (license license:bsd-3)))

(define-public go-rsc-io-pdf
  (package
    (name "go-rsc-io-pdf")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsc/pdf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01qjjwa8nn5a2jzd360xqg5zc8s0i2fpwcn2w2g6y2jgn9wl8x84"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "rsc.io/pdf"))
    (home-page "https://rsc.io/pdf")
    (synopsis #f)
    (description "Package pdf implements reading of PDF files.")
    (license license:bsd-3)))

(define-public go-golang-org-x-arch
  (package
    (name "go-golang-org-x-arch")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/arch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104mnfx3v6lwjndjd35ly8r6yb4bb74lq5sq1cqpxw38mqyzqmx2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/arch"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs (list go-rsc-io-pdf))
    (home-page "https://golang.org/x/arch")
    (synopsis "arch")
    (description
     "This repository holds machine architecture information used by the Go toolchain.
 The parts needed in the main Go repository are copied in.")
    (license license:bsd-3)))

(define-public go-github-com-bytedance-sonic
  (package
    (name "go-github-com-bytedance-sonic")
    (version "1.12.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/bytedance/sonic/archive/refs/tags/v"
             version
             ".tar.gz"))
       (sha256
        (base32 "03bgak6xckrv1r39s9j2kk1kpkfwl3g9g1gklvc9y0wz56mwjgqp"))
     ;; (origin
     ;;   (method git-fetch)
     ;;   (uri (git-reference
     ;;         (url "https://github.com/bytedance/sonic")
     ;;         (commit (string-append "v" version))))
     ;;   (file-name (git-file-name name version))
     ;;   (sha256
     ;;    (base32 "011hp6lvqvx4facxsmy6vya02g9q3rlnmxcii827sbf6bssy7wxp")))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "loader"))))
     )
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/bytedance/sonic"))
    (propagated-inputs (list go-golang-org-x-arch
                             go-github-com-twitchyliquid64-golang-asm
                             go-github-com-stretchr-testify
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-davecgh-go-spew
                             go-github-com-cloudwego-base64x
                             go-github-com-bytedance-sonic-loader
                             ))
    (home-page "https://github.com/bytedance/sonic")
    (synopsis "Sonic")
    (description
     "English |
@@url{https://github.com/bytedance/sonic/blob/v1.12.6/README_ZH_CN.md,中文}.")
    (license license:asl2.0)))

(define-public go-github-com-gin-contrib-sse
  (package
    (name "go-github-com-gin-contrib-sse")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gin-contrib/sse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "072nq91a65n5xvwslqjyvydfd0mfpnvb3vwjyfvmzm1ym96wr1nd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gin-contrib/sse"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/gin-contrib/sse")
    (synopsis "Server-Sent Events")
    (description
     "Server-sent events (SSE) is a technology where a browser receives automatic
updates from a server via HTTP connection.  The Server-Sent Events
@code{EventSource} API is
@@url{http://www.w3.org/TR/2009/WD-eventsource-20091029/,standardized as part of
HTML5[1] by the W3C}.")
    (license license:expat)))

(define-public go-github-com-gin-gonic-gin
  (package
    (name "go-github-com-gin-gonic-gin")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gin-gonic/gin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01xjvw2d46b77jnszgbwqbdzh9jx7y3h5ik3q30y9dn9gaq5mhks"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gin-gonic/gin"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-protobuf
                             go-golang-org-x-net
                             go-github-com-ugorji-go-codec
                             go-github-com-stretchr-testify
                             go-github-com-pelletier-go-toml-v2
                             go-github-com-mattn-go-isatty
                             go-github-com-json-iterator-go
                             go-github-com-goccy-go-json
                             go-github-com-go-playground-validator-v10
                             go-github-com-gin-contrib-sse
                             go-github-com-bytedance-sonic))
    (home-page "https://github.com/gin-gonic/gin")
    (synopsis "Gin Web Framework")
    (description "Package gin implements a HTTP web framework called gin.")
    (license license:expat)))

(define-public go-github-com-cloudykit-fastprinter
  (package
    (name "go-github-com-cloudykit-fastprinter")
    (version "0.0.0-20200109182630-33d98a066a53")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CloudyKit/fastprinter")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06kkykc9rblq64q1bzi2r53r9y7iddi6abx2lhgkas55ld5qsjr9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/CloudyKit/fastprinter"))
    (home-page "https://github.com/CloudyKit/fastprinter")
    (synopsis "fastprinter")
    (description
     "@code{FastPrinter} supports write values in io.Writer without allocation.")
    (license license:expat)))

(define-public go-github-com-cloudykit-jet-v6
  (package
    (name "go-github-com-cloudykit-jet")
    (version "6.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CloudyKit/jet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c3pycn11v6203xs4hd9r1lyh2jsrxx8c6s8kddkaskx0czd4qyv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/CloudyKit/jet"
      #:unpack-path "github.com/CloudyKit/jet"))
    (propagated-inputs (list go-github-com-cloudykit-fastprinter))
    (home-page "https://github.com/CloudyKit/jet")
    (synopsis "Jet Template Engine for Go")
    (description
     "Jet is a template engine developed to be easy to use, powerful, dynamic, yet
secure and very fast.")
    (license license:asl2.0)))

(define-public go-github-com-joker-hpp
  (package
    (name "go-github-com-joker-hpp")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Joker/hpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xnqkjkmqdj48w80qa74rwcmgar8dcilpkcrcn1f53djk45k1gq2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Joker/hpp"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/Joker/hpp")
    (synopsis "HTML Pretty Print for Go (golang)")
    (description
     "Package hpp (github.com/Joker/hpp) is a HTML formatter for Go.")
    (license license:bsd-3)))

(define-public go-github-com-joker-jade
  (package
    (name "go-github-com-joker-jade")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Joker/jade")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nbf738nyrjbs70ikk8crhfq3qh7y2c6mig0qmjysfnlwq2x7dml"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Joker/jade"))
    (propagated-inputs (list go-golang-org-x-tools go-github-com-joker-hpp))
    (home-page "https://github.com/Joker/jade")
    (synopsis "Jade.go - template engine for Go (golang)")
    (description
     "Jade.go - template engine.  Package implements Jade-lang templates for
generating Go html/template output.")
    (license license:bsd-3)))

(define-public go-github-com-shopify-goreferrer
  (package
    (name "go-github-com-shopify-goreferrer")
    (version "0.0.0-20240724165105-aceaa0259138")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shopify/goreferrer")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "055yd7152c102z56f48023nb63xff2w2bcz8vf760zq3hj1qjafx"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:go go-1.22
      #:import-path "github.com/Shopify/goreferrer"))
    (propagated-inputs (list go-golang-org-x-net
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/Shopify/goreferrer")
    (synopsis "goreferrer")
    (description
     "This package provides a Go module that analyzes and classifies different kinds
of referrer URLs (search, social, ...).")
    (license license:expat)))

(define-public go-github-com-dgraph-io-ristretto-0-3
  (package
    (name "go-github-com-dgraph-io-ristretto")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/ristretto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v29j6b0r87pf5dwsxi3rxa8yavkl3h79i63rbddfd42p2jyssyg"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Tests fail on 32 bit architecure:
      ;;
      ;; cannot use 12 << 30 (untyped int constant 12884901888) as int value
      ;; in assignment (overflows).
      ;;
      ;; cannot use 4340958203495 (untyped int constant) as int value in
      ;; argument to z.KeyToHash (overflows)
      #:tests? (and (target-64bit?)
                    (not (%current-target-system)))
      #:import-path "github.com/dgraph-io/ristretto"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks-and-contrib
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "benchmarks")))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-cespare-xxhash
           go-github-com-dgryski-go-farm
           go-github-com-dustin-go-humanize
           go-github-com-golang-glog
           go-github-com-pkg-errors
           go-golang-org-x-sys))
    (home-page "https://github.com/dgraph-io/ristretto")
    (synopsis "Memory-bound cache in Golang")
    (description
     "Ristretto is a concurrent, fixed size, in-memory cache with a dual focus
on throughput and hit ratio performance.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-dgraph-io-badger-v2
  (package
    (name "go-github-com-dgraph-io-badger")
    (version "2.2007.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraph-io/badger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17jnw7rr59gbjs68z9v4vw251qxiv19xwq0cfzqaflppy3w9yfih"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.20
      #:import-path "github.com/dgraph-io/badger"
      #:unpack-path "github.com/dgraph-io/badger"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-net
                             go-github-com-stretchr-testify
                             go-github-com-spf13-cobra
                             go-github-com-pkg-errors
                             go-github-com-klauspost-compress
                             go-github-com-golang-snappy
                             go-github-com-golang-protobuf
                             go-github-com-dustin-go-humanize
                             go-github-com-dgryski-go-farm
                             go-github-com-dgraph-io-ristretto-0-3
                             go-github-com-cespare-xxhash))
    (home-page "https://github.com/dgraph-io/badger")
    (synopsis "BadgerDB")
    (description
     "Package badger implements an embeddable, simple and fast key-value database,
written in pure Go.  It is designed to be highly performant for both reads and
writes simultaneously.  Badger uses Multi-Version Concurrency Control (MVCC),
and supports transactions.  It runs transactions concurrently, with serializable
snapshot isolation guarantees.")
    (license license:asl2.0)))

(define-public go-github-com-flosch-pongo2-v4
  (package
    (name "go-github-com-flosch-pongo2")
    (version "4.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flosch/pongo2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01j1a1njqxnqndnzkv94n178b5sg16v7m16jvpdd790pmv5vc1ii"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/flosch/pongo2"
      #:unpack-path "github.com/flosch/pongo2"))
    (propagated-inputs (list go-gopkg-in-check-v1))
    (home-page "https://github.com/flosch/pongo2")
    (synopsis "2")
    (description "Package pongo2 is a Django-syntax like template-engine.")
    (license license:expat)))

(define-public go-github-com-gomarkdown-markdown
  (package
    (name "go-github-com-gomarkdown-markdown")
    (version "0.0.0-20241205020045-f7e15b2f3e62")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gomarkdown/markdown")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qbgh7xfrpcmvcr9dh5n5qis68icp81zsqi34xvfylapf2s23n7i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/gomarkdown/markdown"))
    (home-page "https://github.com/gomarkdown/markdown")
    (synopsis "Markdown Parser and HTML Renderer for Go")
    (description
     "Package markdown implements markdown parser and HTML renderer.")
    (license license:bsd-2)))

(define-public go-github-com-ajg-form
  (package
    (name "go-github-com-ajg-form")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ajg/form")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d6sxzzf9yycdf8jm5877y0khmhkmhxfw3sc4xpdcsrdlc7gqh5a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/ajg/form"))
    (home-page "https://github.com/ajg/form")
    (synopsis "form")
    (description "Package form implements encoding and decoding of
application/x-www-form-urlencoded data.")
    (license license:bsd-3)))

(define-public go-github-com-fatih-structs
  (package
    (name "go-github-com-fatih-structs")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fatih/structs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wrhb8wp8zpzggl61lapb627lw8yv281abvr6vqakmf569nswa9q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fatih/structs"))
    (home-page "https://github.com/fatih/structs")
    (synopsis "Structs")
    (description
     "Package structs contains various utilities functions to work with structs.")
    (license license:expat)))

(define-public go-github-com-imkira-go-interpol
  (package
    (name "go-github-com-imkira-go-interpol")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/imkira/go-interpol")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "180h3pf2p0pch6hmqf45wk7wd87md83d3p122f8ll43x5nja5mph"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/imkira/go-interpol"))
    (home-page "https://github.com/imkira/go-interpol")
    (synopsis "interpol")
    (description
     "Package interpol provides utility functions for doing format-string like string
interpolation using named parameters.  Currently, a template only accepts
variable placeholders delimited by brace characters (eg. \"Hello {foo} {bar}\").")
    (license license:expat)))

(define-public go-github-com-sanity-io-litter
  (package
    (name "go-github-com-sanity-io-litter")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sanity-io/litter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vgzwl9y50x4wdiaiqdbr4y9h1dvcrhdaljd65dc3ypqcbp0ynhg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sanity-io/litter"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/sanity-io/litter")
    (synopsis "Litter")
    (description "Litter is provided by.")
    (license license:expat)))

(define-public go-github-com-yalp-jsonpath
  (package
    (name "go-github-com-yalp-jsonpath")
    (version "0.0.0-20180802001716-5cc68e5049a0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yalp/jsonpath")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kkyxp1cg3kfxy5hhwzxg132jin4xb492z5jpqq94ix15v6rdf4b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yalp/jsonpath"))
    (home-page "https://github.com/yalp/jsonpath")
    (synopsis "jsonpath")
    (description
     "Package jsonpath implements Stefan Goener's JSONPath
@@url{http://goessner.net/articles/@code{JsonPath/,http://goessner.net/articles/JsonPath}/}.")
    (license license:bsd-3)))

(define-public go-github-com-sergi-go-diff-diffmatchpatch
  (package
    (name "go-github-com-sergi-go-diff-diffmatchpatch")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sergi/go-diff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7lsa3kjxbrx66r93d0pvx1408b80ignpi39fzka1qc0ylshw32"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sergi/go-diff/diffmatchpatch"
      #:unpack-path "github.com/sergi/go-diff"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/sergi/go-diff")
    (synopsis "Diff, match and patch text in Golang")
    (description
     "This package provides algorithms to perform operations to synchronizing
plain text, like comparing two texts and return their differences, perform
fuzzy matching of text, and applying patches onto text.")
    (license license:expat)))

(define-public go-github-com-yudai-golcs
  (let ((commit "ecda9a501e8220fae3b4b600c3db4b0ba22cfc68")
        (revision "0"))
    (package
      (name "go-github-com-yudai-golcs")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yudai/golcs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mx6wc5fz05yhvg03vvps93bc5mw4vnng98fhmixd47385qb29pq"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/yudai/golcs"))
      (home-page "https://github.com/yudai/golcs")
      (synopsis "Calculate @acronym{LCS, longest common sequence} in Golang")
      (description
       "This package provides functions to calculate @acronym{LCS, longest
common sequence} values from two arbitrary arrays.")
      (license license:expat))))

(define-public go-github-com-yudai-gojsondiff
  (package
    (name "go-github-com-yudai-gojsondiff")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yudai/gojsondiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qnymi0027mb8kxm24mmd22bvjrdkc56c7f4q3lbdf93x1vxbbc2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/yudai/gojsondiff"))
    (propagated-inputs (list go-github-com-sergi-go-diff-diffmatchpatch
                             go-github-com-yudai-golcs
                             go-github-com-onsi-ginkgo
                             ))
    (home-page "https://github.com/yudai/gojsondiff")
    (synopsis "Go JSON Diff (and Patch)")
    (description
     "Package gojsondiff implements \"Diff\" that compares two JSON objects and
generates Deltas that describes differences between them.  The package also
provides \"Patch\" that apply Deltas to a JSON object.")
    (license license:expat)))

(define-public go-github-com-tailscale-depaware
  (package
    (name "go-github-com-tailscale-depaware")
    (version "0.0.0-20241028160002-3d7f3b30ed0e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tailscale/depaware")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4y346y6c3vnzv639qhca8bwb936zl3mb25xzncj4d809nz2n9p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tailscale/depaware"))
    (propagated-inputs (list go-golang-org-x-tools go-github-com-pkg-diff))
    (home-page "https://github.com/tailscale/depaware")
    (synopsis "depaware")
    (description
     "The depaware command makes you aware of your dependencies by putting them in
your face in git and during code review.")
    (license license:bsd-3)))

(define-public go-moul-io-http2curl-v2
  (package
    (name "go-moul-io-http2curl")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moul/http2curl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yh07sqj31dg1rm46akp91m953mrxnr2l4sz7wjwqsw3z501c1fk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "moul.io/http2curl"))
    (propagated-inputs (list go-github-com-tailscale-depaware))
    (home-page "https://moul.io/http2curl")
    (synopsis "http2curl")
    (description "📐 Convert Golang's http.Request to CURL command line.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-iris-contrib-httpexpect-v2
  (package
    (name "go-github-com-iris-contrib-httpexpect")
    (version "2.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iris-contrib/httpexpect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lnq6g6kdc0avgn114dgabccsa5hi03iy24c4r9595v7qs6nhq3v"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/iris-contrib/httpexpect"
      #:unpack-path "github.com/iris-contrib/httpexpect"))
    (propagated-inputs (list go-moul-io-http2curl-v2
                             go-golang-org-x-net
                             go-github-com-yudai-gojsondiff
                             go-github-com-yalp-jsonpath
                             go-github-com-xeipuuv-gojsonschema
                             go-github-com-stretchr-testify
                             go-github-com-sanity-io-litter
                             go-github-com-mitchellh-go-wordwrap
                             go-github-com-mattn-go-isatty
                             go-github-com-imkira-go-interpol
                             go-github-com-gorilla-websocket
                             go-github-com-google-go-querystring
                             go-github-com-gobwas-glob
                             go-github-com-fatih-structs
                             go-github-com-fatih-color
                             go-github-com-ajg-form))
    (home-page "https://github.com/iris-contrib/httpexpect")
    (synopsis "HTTP Expect")
    (description
     "Package httpexpect helps with end-to-end HTTP and REST API testing.")
    (license license:expat)))

(define-public go-github-com-iris-contrib-schema
  (package
    (name "go-github-com-iris-contrib-schema")
    (version "0.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iris-contrib/schema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0njlpmbb9pdkkl9ikmyi07h03zz4900w7986nfk0398w9bm3hffs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/iris-contrib/schema"))
    (home-page "https://github.com/iris-contrib/schema")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-kataras-blocks
  (package
    (name "go-github-com-kataras-blocks")
    (version "0.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/blocks")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07ka87lk19g07fjmkqrjc9hpw36zgybr4rcljya6xa4i008ynxkf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/kataras/blocks"))
    (propagated-inputs (list go-github-com-valyala-bytebufferpool
                             go-github-com-russross-blackfriday-v2))
    (home-page "https://github.com/kataras/blocks")
    (synopsis "Blocks")
    (description
     "Blocks is a, simple, Go-idiomatic view engine based on
@@url{https://pkg.go.dev/html/template?tab=doc#Template,html/template}, plus the
following features:.")
    (license license:expat)))

(define-public go-github-com-kataras-jwt
  (package
    (name "go-github-com-kataras-jwt")
    (version "0.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ch82mkz3245p36zm77wv1xbfywr3pyg88yd8imc0d7y16qhfjw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/jwt"))
    (home-page "https://github.com/kataras/jwt")
    (synopsis "JWT")
    (description
     "Package jwt aims to provide an implementation of the JSON Web Token standard.
The library supports the JSON Web Algorithm standard with HMAC, RSA, ECDSA and
@code{EdDSA}.  The signing operation can accept multiple claims and merge as
one, not a single change to the existing structs is required.  The verification
process performs all the standard validations out of the box.  The library
supports only the compact serialization format.")
    (license license:expat)))

(define-public go-github-com-gobwas-httphead
  (package
    (name "go-github-com-gobwas-httphead")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/httphead")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "106l8ml5yihld3rrf45q5fhlsx64hrpj2dsvnnm62av4ya5nf0gb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/httphead"))
    (home-page "https://github.com/gobwas/httphead")
    (synopsis "httphead.")
    (description
     "Package httphead contains utils for parsing HTTP and HTTP-grammar compatible
text protocols headers.")
    (license license:expat)))

(define-public go-github-com-gobwas-pool
  (package
    (name "go-github-com-gobwas-pool")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/pool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0imipsf8nslc78in78wcri2ir2zzajp2h543dp2cshrrdbwkybx7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/pool"))
    (home-page "https://github.com/gobwas/pool")
    (synopsis "pool")
    (description
     "Package pool contains helpers for pooling structures distinguishable by size.")
    (license license:expat)))

(define-public go-github-com-gobwas-ws
  (package
    (name "go-github-com-gobwas-ws")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobwas/ws")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nqgb75cizx11igwjqx6b6mlzl7yxy6x683m9aaalgcx9n1qxan7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gobwas/ws"))
    (propagated-inputs (list go-github-com-gobwas-pool
                             go-github-com-gobwas-httphead))
    (home-page "https://github.com/gobwas/ws")
    (synopsis "ws")
    (description
     "Package ws implements a client and server for the @code{WebSocket} protocol as
specified in @@url{https://rfc-editor.org/rfc/rfc6455.html,RFC 6455}.")
    (license license:expat)))

(define-public go-github-com-iris-contrib-go-uuid
  (package
    (name "go-github-com-iris-contrib-go-uuid")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iris-contrib/go.uuid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nc0ggn0a6bcwdrwinnx3z6889x65c20a2dwja0n8can3xblxs35"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/iris-contrib/go.uuid"))
    (home-page "https://github.com/iris-contrib/go.uuid")
    (synopsis "UUID package for Go language")
    (description
     "Package uuid provides implementation of Universally Unique Identifier (UUID).
Supported versions are 1, 3, 4 and 5 (as specified in
@@url{https://rfc-editor.org/rfc/rfc4122.html,RFC 4122}) and version 2 (as
specified in DCE 1.1).")
    (license license:expat)))

(define-public go-github-com-mediocregopher-radix-v3
  (package
    (name "go-github-com-mediocregopher-radix")
    (version "3.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mediocregopher/radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "084nif0hk0dcn1zkd3svack3rki061srw8gx1jx9szhjbgk5ax7n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/mediocregopher/radix"
      #:unpack-path "github.com/mediocregopher/radix"))
    (propagated-inputs (list go-golang-org-x-xerrors
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/mediocregopher/radix")
    (synopsis "Radix")
    (description
     "Package radix implements all functionality needed to work with redis and all
things related to it, including redis cluster, pubsub, sentinel, scanning, lua
scripting, and more.")
    (license license:expat)))

(define-public go-github-com-kataras-neffos
  (package
    (name "go-github-com-kataras-neffos")
    (version "0.0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/neffos")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rsm0kp6xdv8bqa3a26rdxxj9xfdcmslffmahb9962psrg8kgylf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/neffos"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-nats-io-nats-go
                             go-github-com-mediocregopher-radix-v3
                             go-github-com-iris-contrib-go-uuid
                             go-github-com-gorilla-websocket
                             go-github-com-gobwas-ws))
    (home-page "https://github.com/kataras/neffos")
    (synopsis "About neffos")
    (description
     "Neffos is a cross-platform real-time framework with expressive, elegant API
written in @@url{https://go.dev,Go}.  Neffos takes the pain out of development
by easing common tasks used in real-time backend and frontend applications such
as:.")
    (license license:expat)))

(define-public go-github-com-kataras-sitemap
  (package
    (name "go-github-com-kataras-sitemap")
    (version "0.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/sitemap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i2b1h8759j1505lifpz259w72wag23lrfc3wdkrxmck03mifq1n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/sitemap"))
    (home-page "https://github.com/kataras/sitemap")
    (synopsis "Sitemap (Go)")
    (description
     "Package sitemap implements the Sitemap Protocol.  Reference:
@@url{https://www.sitemaps.org/protocol.html,https://www.sitemaps.org/protocol.html}.")
    (license license:expat)))

(define-public go-github-com-kataras-tunnel
  (package
    (name "go-github-com-kataras-tunnel")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/tunnel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j9xjmcg6fpc2g4x4arr6bal4g58l5zxvn5nqq26b9dj76l84dqc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/kataras/tunnel"))
    (home-page "https://github.com/kataras/tunnel")
    (synopsis "Tunnel")
    (description "Public URLs for exposing your local web server using
@@url{https://ngrok.com/,ngrok's API}.")
    (license license:expat)))

(define-public go-github-com-mailgun-raymond-v2
  (package
    (name "go-github-com-mailgun-raymond")
    (version "2.0.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mailgun/raymond")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zg22w15avn2w0mv8qzzm6xyjvz942iqfd3dsqj2hy7x69nsyb8w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/mailgun/raymond"
      #:unpack-path "github.com/mailgun/raymond"))
    (propagated-inputs (list go-gopkg-in-yaml-v2
                             go-github-com-sirupsen-logrus))
    (home-page "https://github.com/mailgun/raymond")
    (synopsis "raymond")
    (description "Package raymond provides handlebars evaluation.")
    (license license:expat)))

(define-public go-github-com-bsm-ginkgo-v2
  (package
    (name "go-github-com-bsm-ginkgo")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01k1j1bwdq23hs9zzbz9kdljvr6hzym53mqxh2gy0bz4lggcd6qs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/ginkgo"
      #:unpack-path "github.com/bsm/ginkgo"))
    (home-page "https://github.com/bsm/ginkgo")
    (synopsis "Ginkgo")
    (description
     "Ginkgo is a testing framework for Go designed to help you write expressive
tests. @@url{https://github.com/onsi/ginkgo,https://github.com/onsi/ginkgo}
MIT-Licensed.")
    (license license:expat)))

(define-public go-github-com-bsm-gomega
  (package
    (name "go-github-com-bsm-gomega")
    (version "1.27.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7p85wsqv1j9aq052vdw006xq42n1rdgnk1lr6f5wnapwab2shz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/gomega"))
    (home-page "https://github.com/bsm/gomega")
    (synopsis "Gomega")
    (description
     "Gomega is the Ginkgo BDD-style testing framework's preferred matcher library.")
    (license license:expat)))

(define-public go-github-com-dgryski-go-rendezvous
  (package
    (name "go-github-com-dgryski-go-rendezvous")
    (version "0.0.0-20200823014737-9f7001d12a5f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-rendezvous")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hhdbsm5k19kh1fyxs4aibza9jylils4p3555lr8xalhj2iz3zlz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-rendezvous"))
    (home-page "https://github.com/dgryski/go-rendezvous")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-redis-go-redis-v9
  (package
    (name "go-github-com-redis-go-redis")
    (version "9.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sqa2zjkq6jcn7bg5da863j5275q4hrgrsqbmwxr0j75kxfhw9dx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/redis/go-redis"
      #:unpack-path "github.com/redis/go-redis"))
    (propagated-inputs (list go-github-com-dgryski-go-rendezvous
                             go-github-com-cespare-xxhash-v2
                             go-github-com-bsm-gomega
                             go-github-com-bsm-ginkgo-v2))
    (home-page "https://github.com/redis/go-redis")
    (synopsis "Redis client for Go")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-schollz-closestmatch
  (package
    (name "go-github-com-schollz-closestmatch")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schollz/closestmatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gpzqrvka1bps790b2px0h9z1j2vjga5xx7204anl83qndiyyrli"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/schollz/closestmatch"))
    (home-page "https://github.com/schollz/closestmatch")
    (synopsis "closestmatch 📃")
    (description
     "is a simple and fast Go library for fuzzy matching an input string to a list of
target strings.  is useful for handling input from a user where the input (which
could be mispelled or out of order) needs to match a key in a database.  uses a
@@url{https://en.wikipedia.org/wiki/Bag-of-words_model,bag-of-words approach} to
precompute character n-grams to represent each possible target string.  The
closest matches have highest overlap between the sets of n-grams.  The
precomputation scales well and is much faster and more accurate than Levenshtein
for long strings.")
    (license license:expat)))

(define-public go-github-com-vmihailenco-tagparser-v2
  (package
    (name "go-github-com-vmihailenco-tagparser")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/tagparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13arliaz3b4bja9jj7cr5ax4zvxaxm484fwrn0q6d6jjm1l35m1k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vmihailenco/tagparser"
      #:unpack-path "github.com/vmihailenco/tagparser"))
    (home-page "https://github.com/vmihailenco/tagparser")
    (synopsis "Opinionated Golang tag parser")
    (description "Install:.")
    (license license:bsd-2)))

(define-public go-github-com-vmihailenco-msgpack-v5
  (package
    (name "go-github-com-vmihailenco-msgpack")
    (version "5.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmihailenco/msgpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vkyyywvip0vwjmnmnmkl9hz345k54nigj2mq8fbbjx96xpgghvz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/vmihailenco/msgpack"
      #:unpack-path "github.com/vmihailenco/msgpack"))
    (propagated-inputs (list go-github-com-vmihailenco-tagparser-v2
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/vmihailenco/msgpack")
    (synopsis "MessagePack encoding for Golang")
    (description
     "msgpack supports 2 last Go versions and requires support for
@@url{https://github.com/golang/go/wiki/Modules,Go modules}.  So make sure to
initialize a Go module:.")
    (license license:bsd-2)))

(define-public go-github-com-yosssi-ace
  (package
    (name "go-github-com-yosssi-ace")
    (version "0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yosssi/ace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kbvbc56grrpnl65grygd23gyn3nkkhxdg8awhzkjmd0cvki8w1f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/yosssi/ace"))
    (home-page "https://github.com/yosssi/ace")
    (synopsis "Ace - HTML template engine for Go")
    (description "Package ace provides an HTML template engine.")
    (license license:expat)))

(define-public go-github-com-kataras-iris-v12
  (package
    (name "go-github-com-kataras-iris")
    (version "12.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kataras/iris")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nc28w89q6sx489a6bk2jp27k5z1yf4a8w1jkj08rsd1jq7ld4jy"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:tests? #f
      #:go go-1.22
      #:import-path "github.com/kataras/iris"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-gopkg-in-ini-v1
                             ;; go-google-golang-org-protobuf
                             go-golang-org-x-time
                             go-golang-org-x-text
                             go-golang-org-x-sys
                             go-golang-org-x-net
                             go-golang-org-x-exp
                             go-golang-org-x-crypto
                             go-go-etcd-io-bbolt
                             go-github-com-yosssi-ace
                             go-github-com-vmihailenco-msgpack-v5
                             go-github-com-tdewolff-minify-v2
                             go-github-com-shirou-gopsutil-v3
                             go-github-com-schollz-closestmatch
                             go-github-com-redis-go-redis-v9
                             go-github-com-microcosm-cc-bluemonday
                             go-github-com-mailru-easyjson
                             go-github-com-mailgun-raymond-v2
                             go-github-com-klauspost-compress
                             go-github-com-kataras-tunnel
                             go-github-com-kataras-sitemap
                             go-github-com-kataras-pio
                             go-github-com-kataras-neffos
                             go-github-com-kataras-jwt
                             go-github-com-kataras-golog
                             go-github-com-kataras-blocks
                             go-github-com-json-iterator-go
                             go-github-com-iris-contrib-schema
                             go-github-com-iris-contrib-httpexpect-v2
                             go-github-com-gorilla-securecookie
                             go-github-com-google-uuid
                             go-github-com-gomarkdown-markdown
                             go-github-com-golang-snappy
                             go-github-com-flosch-pongo2-v4
                             go-github-com-fatih-structs
                             go-github-com-dgraph-io-badger-v2
                             go-github-com-blang-semver-v4
                             go-github-com-andybalholm-brotli
                             go-github-com-shopify-goreferrer
                             go-github-com-joker-jade
                             go-github-com-cloudykit-jet-v6
                             go-github-com-burntsushi-toml))
    (home-page "https://github.com/kataras/iris")
    (synopsis "Iris Web Framework")
    (description
     "Package iris implements the highest realistic performance, easy to learn Go web
framework.  Iris provides a beautifully expressive and easy to use foundation
for your next website, API, or distributed app.  Low-level handlers compatible
with `net/http` and high-level fastest MVC implementation and handlers
dependency injection.  Easy to learn for new gophers and advanced features for
experienced, it goes as far as you dive into it!")
    (license license:bsd-3)))

(define-public go-github-com-labstack-gommon
  (package
    (name "go-github-com-labstack-gommon")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/labstack/gommon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05mafzmx050hc3js3i0h05ga81kk3hhhlv395xwzv9n38h27xpnz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/labstack/gommon"))
    (propagated-inputs (list go-github-com-valyala-fasttemplate
                             go-github-com-stretchr-testify
                             go-github-com-mattn-go-isatty
                             go-github-com-mattn-go-colorable))
    (home-page "https://github.com/labstack/gommon")
    (synopsis "Gommon")
    (description "Common packages for Go.")
    (license license:expat)))

(define-public go-github-com-valyala-fasttemplate
  (package
    (name "go-github-com-valyala-fasttemplate")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fasttemplate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12hywkz2mfvxzfpgabc53bm4jkxxmcssrr0k4wxzzrnv0v7mj6bj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fasttemplate"))
    (propagated-inputs (list go-github-com-valyala-bytebufferpool))
    (home-page "https://github.com/valyala/fasttemplate")
    (synopsis "fasttemplate")
    (description
     "Package fasttemplate implements simple and fast template library.")
    (license license:expat)))

(define-public go-github-com-labstack-echo-v4
  (package
    (name "go-github-com-labstack-echo")
    (version "4.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/labstack/echo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i4w36f1k17bykc24dzr2ng5zpsyysfg5bzfvlbrphxxzhsngxdy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/labstack/echo"
      #:unpack-path "github.com/labstack/echo"))
    (propagated-inputs (list go-golang-org-x-time
                             go-golang-org-x-net
                             go-golang-org-x-crypto
                             go-github-com-valyala-fasttemplate
                             go-github-com-stretchr-testify
                             go-github-com-labstack-gommon))
    (home-page "https://github.com/labstack/echo")
    (synopsis "Echo")
    (description
     "Package echo implements high performance, minimalist Go web framework.")
    (license license:expat)))

(define-public go-github-com-oapi-codegen-runtime
  (package
    (name "go-github-com-oapi-codegen-runtime")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oapi-codegen/runtime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1win3ksak8q2nm78id1di33s72ld476bvbyjwwrg0h7016jz66qr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      ;; #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/oapi-codegen/runtime"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-labstack-echo-v4
                             go-github-com-kataras-iris-v12
                             go-github-com-google-uuid
                             go-github-com-gin-gonic-gin
                             go-github-com-apapsch-go-jsonmerge-v2))
    (home-page "https://github.com/oapi-codegen/runtime")
    (synopsis "oapi-codegen/runtime")
    (description "Copyright 2021 @code{DeepMap}, Inc.")
    (license license:asl2.0)))

(define-public go-github-com-influxdata-influxdb-client-go-v2
  (package
    (name "go-github-com-influxdata-influxdb-client-go")
    (version "2.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/influxdata/influxdb-client-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bci1j2nf1z9x6b08cd336hjjfms6mn6g7syxmy0mcd563s9bch7"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/influxdata/influxdb-client-go"))
    (propagated-inputs (list go-golang-org-x-net
                             go-github-com-stretchr-testify
                             go-github-com-oapi-codegen-runtime
                             go-github-com-influxdata-line-protocol))
    (home-page "https://github.com/influxdata/influxdb-client-go")
    (synopsis "InfluxDB Client Go")
    (description
     "Package influxdb2 provides API for using @code{InfluxDB} client in Go.  It's
intended to use with @code{InfluxDB} 2 server. @code{WriteAPI}, @code{QueryAPI}
and Health work also with @code{InfluxDB} 1.8.")
    (license license:expat)))

(define-public go-github-com-go-ole-go-ole
  (package
    (name "go-github-com-go-ole-go-ole")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ole/go-ole")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vr62wwjp206sxah2l79l007s7n187fjzkrnwb85ivqmazfjspxl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ole/go-ole"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/go-ole/go-ole")
    (synopsis "Go OLE")
    (description
     "Go bindings for Windows COM using shared libraries instead of cgo.")
    (license license:expat)))

(define-public go-github-com-jaypipes-pcidb
  (package
    (name "go-github-com-jaypipes-pcidb")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaypipes/pcidb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "092wax6i7bn2lvwbz0rbbj17ly60n4b4n5ymy92w4d0mzxrn1ac4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/jaypipes/pcidb"))
    (propagated-inputs (list go-github-com-mitchellh-go-homedir))
    (home-page "https://github.com/jaypipes/pcidb")
    (synopsis "- the Golang PCI DB library")
    (description
     "@@code{pcidb} is a small Golang library for programmatic querying of PCI vendor,
product and class information.")
    (license (list license:asl2.0 license:asl2.0))))

(define-public go-github-com-jaypipes-ghw
  (package
    (name "go-github-com-jaypipes-ghw")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaypipes/ghw")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c4fjq06c28cdfrdc05mf0ari9xxd9smfaympw4ba0jpcr9z9phc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/jaypipes/ghw"))
    (propagated-inputs (list go-howett-net-plist
                             go-gopkg-in-yaml-v3
                             go-github-com-spf13-cobra
                             go-github-com-pkg-errors
                             go-github-com-jaypipes-pcidb))
    (home-page "https://github.com/jaypipes/ghw")
    (synopsis "- Go HardWare discovery/inspection library")
    (description
     "package ghw discovers hardware-related information about the host computer,
including CPU, memory, block storage, NUMA topology, network devices, PCI, GPU,
and baseboard/BIOS/chassis/product information.")
    (license license:asl2.0)))

(define-public go-github-com-samber-lo
  (package
    (name "go-github-com-samber-lo")
    (version "1.47.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/samber/lo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0grpzg4dirb3szd1miwx38hq3gzdxw9a9l94s5wdcgg5ak2xxicc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/samber/lo"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/samber/lo")
    (synopsis "lo - Iterate over slices, maps, channels...")
    (description "✨.")
    (license license:expat)))

(define-public go-github-com-jinzhu-inflection
  (package
    (name "go-github-com-jinzhu-inflection")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/inflection")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "165i20d11s03771gi43skl66salxj36212r25fbs0cgr4qgfj7fy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jinzhu/inflection"))
    (home-page "https://github.com/jinzhu/inflection")
    (synopsis "Inflection")
    (description
     "Package inflection pluralizes and singularizes English nouns.")
    (license license:expat)))

(define-public go-github-com-jinzhu-now
  (package
    (name "go-github-com-jinzhu-now")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jinzhu/now")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ywpaxs6d3y8gqlzx6rh3yw4ya83bnx0hrs0k0wq5bxbjhfmlil"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jinzhu/now"))
    (home-page "https://github.com/jinzhu/now")
    (synopsis "Now")
    (description "Package now is a time toolkit for golang.")
    (license license:expat)))

(define-public go-gorm-io-gorm
  (package
    (name "go-gorm-io-gorm")
    (version "1.25.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gorm/gorm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dy1nxnjm2wr27q9ys17q06c9n84c286ha3715xjhyjbv7khiqzv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "gorm.io/gorm"))
    (propagated-inputs (list go-golang-org-x-text go-github-com-jinzhu-now
                             go-github-com-jinzhu-inflection))
    (home-page "https://gorm.io/gorm")
    (synopsis "GORM")
    (description
     "The fantastic ORM library for Golang, aims to be developer friendly.")
    (license license:expat)))

(define-public go-github-com-analogj-scrutiny-collector
  (package
    (name "go-github-com-analogj-scrutiny-collector")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AnalogJ/scrutiny")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n5hqmwb29mdqphzn55bpq5rgcdnmk3mvs23z981c4h8vfnkk1as"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/analogj/scrutiny/collector/cmd/collector-metrics"
      #:unpack-path "github.com/analogj/scrutiny"))
    (propagated-inputs (list go-gorm-io-gorm
                             go-golang-org-x-sync
                             go-github-com-urfave-cli-v2
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-sirupsen-logrus
                             go-github-com-samber-lo
                             go-github-com-mitchellh-mapstructure
                             go-github-com-jaypipes-ghw
                             go-github-com-influxdata-influxdb-client-go-v2
                             go-github-com-golang-mock
                             go-github-com-go-gormigrate-gormigrate-v2
                             go-github-com-glebarez-sqlite
                             go-github-com-gin-gonic-gin
                             go-github-com-fatih-color
                             go-github-com-containrrr-shoutrrr
                             go-github-com-analogj-go-util))
    (inputs (list smartmontools))
    (home-page "https://github.com/analogj/scrutiny")
    (synopsis "scrutiny")
    (description "@code{WebUI} for smartd S.M.A.R.T monitoring.")
    (license license:expat)))

(define-public go-github-com-analogj-scrutiny-web
  (package
    (name "go-github-com-analogj-scrutiny-web")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AnalogJ/scrutiny")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n5hqmwb29mdqphzn55bpq5rgcdnmk3mvs23z981c4h8vfnkk1as"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/analogj/scrutiny/webapp/backend/cmd/scrutiny"
      #:unpack-path "github.com/analogj/scrutiny"))
    (propagated-inputs (list go-gorm-io-gorm
                             go-golang-org-x-sync
                             go-github-com-urfave-cli-v2
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-sirupsen-logrus
                             go-github-com-samber-lo
                             go-github-com-mitchellh-mapstructure
                             go-github-com-jaypipes-ghw
                             go-github-com-influxdata-influxdb-client-go-v2
                             go-github-com-golang-mock
                             go-github-com-go-gormigrate-gormigrate-v2
                             go-github-com-glebarez-sqlite
                             go-github-com-gin-gonic-gin
                             go-github-com-fatih-color
                             go-github-com-containrrr-shoutrrr
                             go-github-com-analogj-go-util))
    (home-page "https://github.com/analogj/scrutiny")
    (synopsis "scrutiny")
    (description "@code{WebUI} for smartd S.M.A.R.T monitoring.")
    (license license:expat)))
