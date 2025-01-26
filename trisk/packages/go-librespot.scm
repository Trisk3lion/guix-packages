(define-module (trisk packages go-librespot)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web))

(define-public go-github-com-devgianlu-shannon
  (package
    (name "go-github-com-devgianlu-shannon")
    (version "0.0.0-20230613115856-82ec90b7fa7e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/devgianlu/shannon")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xz77v50fknm60z5y6nhm209vfwp18ka0b2bfzk1lx8fmwv33s4b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/devgianlu/shannon"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/devgianlu/shannon")
    (synopsis "shannon")
    (description
     "Pure Go implementation of Shannon stream cipher.  No-brainer port of
@@url{https://github.com/plietar/rust-shannon,rust-shannon}.")
    (license license:expat)))

(define-public go-github-com-cenkalti-backoff
  (package
    (name "go-github-com-cenkalti-backoff")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cenkalti/backoff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mf4lsl3rbb8kk42x0mrhzzy4ikqy0jf6nxpzhkr02rdgwh6rjk8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cenkalti/backoff"))
    (home-page "https://github.com/cenkalti/backoff")
    (synopsis "Exponential Backoff")
    (description
     "Package backoff implements backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-github-com-grandcat-zeroconf
  (package
    (name "go-github-com-grandcat-zeroconf")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grandcat/zeroconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02b16pq2mqc6r3rwld4kvh1rq4rdd1wvgq0h12w9f1mx8vq5dr19"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/grandcat/zeroconf"))
    (propagated-inputs (list go-golang-org-x-net go-github-com-pkg-errors
                             go-github-com-miekg-dns
                             go-github-com-cenkalti-backoff))
    (home-page "https://github.com/grandcat/zeroconf")
    (synopsis "ZeroConf: Service Discovery with mDNS")
    (description
     "Package zeroconf is a pure Golang library that employs Multicast DNS-SD for
browsing and resolving services in your network and registering own services in
the local network.")
    (license license:expat)))

(define-public go-github-com-jfreymuth-pulse
  (package
    (name "go-github-com-jfreymuth-pulse")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jfreymuth/pulse")
             (commit "4ffb35054b53c3d59304f99f68b6f8ad80e26ac4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qic6fgjwdl5v2f89jbs8l2ifv47kb0h2xpmpgy7v7l8wb220gm5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jfreymuth/pulse"))
    (home-page "https://github.com/jfreymuth/pulse")
    (synopsis "pulse")
    (description
     "Package pulse implements the pulseaudio protocol in pure go.")
    (license license:expat)))

(define-public go-github-com-knadh-koanf-parsers-yaml
  (package
    (name "go-github-com-knadh-koanf-parsers-yaml")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knadh/koanf")
             (commit (go-version->git-ref version
                                          #:subdir "parsers/yaml"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fnn55c639m56mrsqdlybkpc3kw0z760nc1rlk7ac82zs70q1pkr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knadh/koanf/parsers/yaml"
      #:unpack-path "github.com/knadh/koanf"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/knadh/koanf")
    (synopsis #f)
    (description
     "Package yaml implements a koanf.Parser that parses YAML bytes as conf maps.")
    (license license:expat)))

(define-public go-github-com-knadh-koanf-providers-confmap
  (package
    (name "go-github-com-knadh-koanf-providers-confmap")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knadh/koanf")
             (commit (go-version->git-ref version
                                          #:subdir "providers/confmap"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fnn55c639m56mrsqdlybkpc3kw0z760nc1rlk7ac82zs70q1pkr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knadh/koanf/providers/confmap"
      #:unpack-path "github.com/knadh/koanf"))
    (home-page "https://github.com/knadh/koanf")
    (synopsis #f)
    (description
     "Package confmap implements a koanf.Provider that takes nested and flat
map[string]interface{} config maps and provides them to koanf.")
    (license license:expat)))

(define-public go-github-com-knadh-koanf-providers-file
  (package
    (name "go-github-com-knadh-koanf-providers-file")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knadh/koanf")
             (commit (go-version->git-ref version
                                          #:subdir "providers/file"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vkxdhkw5zrdg9rc0z0dar0830zfgly2gg8z561vibwpp15f22v7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knadh/koanf/providers/file"
      #:unpack-path "github.com/knadh/koanf"))
    (propagated-inputs (list go-github-com-fsnotify-fsnotify))
    (home-page "https://github.com/knadh/koanf")
    (synopsis #f)
    (description
     "Package file implements a koanf.Provider that reads raw bytes from files on disk
to be used with a koanf.Parser to parse into conf maps.")
    (license license:expat)))

(define-public go-github-com-go-viper-mapstructure
  (package
    (name "go-github-com-go-viper-mapstructure")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-viper/mapstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x13x2s0vkikmn5wcarxskhr6c90s64nkbsgjsh7g9sh4v31n5yw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-viper/mapstructure"
      #:unpack-path "github.com/go-viper/mapstructure"))
    (home-page "https://github.com/go-viper/mapstructure")
    (synopsis "mapstructure")
    (description
     "Package mapstructure exposes functionality to convert one arbitrary Go type into
another, typically to convert a map[string]interface{} into a native Go
structure.")
    (license license:expat)))

(define-public go-github-com-knadh-koanf
  (package
    (name "go-github-com-knadh-koanf")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knadh/koanf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g56bx7wspqyzwky17cgg0d6kg3m75klccvfkpdbrvcsh8birns9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/knadh/koanf"
      #:unpack-path "github.com/knadh/koanf"))
    (propagated-inputs (list go-github-com-mitchellh-copystructure
                             go-github-com-mitchellh-reflectwalk
                             go-github-com-go-viper-mapstructure))
    (home-page "https://github.com/knadh/koanf")
    (synopsis "Installation")
    (description
     "@@strong{koanf} is a library for reading configuration from different sources in
different formats in Go applications.  It is a cleaner, lighter
@@url{#readme-alternative-to-viper,alternative to spf13/viper} with better
abstractions and extensibility and far fewer dependencies.")
    (license license:expat)))

(define-public go-github-com-xlab-vorbis-go
  (package
    (name "go-github-com-xlab-vorbis-go")
    (version "0.0.0-20210911202351-b5b85f1ec645")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xlab/vorbis-go")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pqmwhvrakdld8wc76arjnk0mwk4w8my8gm254v0wm77cjl38gc6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/xlab/vorbis-go"
      #:unpack-path "github.com/xlab/vorbis-go"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (delete 'check))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/xlab/vorbis-go")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-devgianlu-go-librespot
  (package
    (name "go-github-com-devgianlu-go-librespot")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/devgianlu/go-librespot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14xpxk63m6vx36vp4qaiyf4h4rigpsxqxyx7wlzzwmvqph0fn64l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:go go-1.22
      #:import-path "github.com/devgianlu/go-librespot/cmd/daemon"
      #:unpack-path "github.com/devgianlu/go-librespot"))
    (native-inputs (list pkg-config libogg libvorbis alsa-lib))
    (propagated-inputs (list go-nhooyr-io-websocket
                             go-google-golang-org-protobuf
                             go-golang-org-x-oauth2
                             go-github-com-fsnotify-fsnotify
                             go-golang-org-x-net
                             go-golang-org-x-exp
                             go-golang-org-x-crypto
                             go-github-com-xlab-vorbis-go
                             go-github-com-spf13-pflag
                             go-github-com-sirupsen-logrus
                             go-github-com-rs-cors
                             go-github-com-knadh-koanf
                             go-github-com-knadh-koanf-parsers-yaml
                             go-github-com-jfreymuth-pulse
                             go-github-com-grandcat-zeroconf
                             go-github-com-gofrs-flock
                             go-github-com-devgianlu-shannon
                             go-github-com-cenkalti-backoff-v4))
    (home-page "https://github.com/devgianlu/go-librespot")
    (synopsis "go-librespot")
    (description
     "Yet another open-source Spotify Connect compatible client, written in Go.")
    (license license:gpl3)))
