(define-module (trisk packages librespot)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages shells)
  #:use-module ((guix licenses) #:prefix license:)
  )


(define-public rust-hyper-util-0.10
  (package
    (name "rust-hyper-util")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; could not find `client` in `hyper_util`
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-http-body-util" ,rust-http-body-util-0.1)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-pnet-datalink" ,rust-pnet-datalink-0.34)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://hyper.rs")
    (synopsis "@code{hyper} utilities")
    (description "This package provides utilities for the @code{hyper} crate.")
    (license license:expat)))

(define-public rust-multimap-0.10
  (package
    (name "rust-multimap")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "multimap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00vs2frqdhrr8iqx4y3fbq73ax5l12837fvbjrpi729d85alrz6y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/havarnov/multimap")
    (synopsis "multimap implementation.")
    (description "This package provides a multimap implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-if-addrs-0.12
  (package
    (name "rust-if-addrs")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "if-addrs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j0c5xzzbfcb3k97zb050cygikfxv47mpj1hlyyyr249qglk6amv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "Return interface IP addresses on Posix and windows systems")
    (description
     "This package provides Return interface IP addresses on Posix and windows systems.")
    (license (list license:expat license:bsd-3))))

(define-public rust-libmdns-0.9
  (package
    (name "rust-libmdns")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libmdns" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qjq77vbmpspq943z686kfrb1jjz6vicws8v8cri848vw6cld1a8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hostname" ,rust-hostname-0.4)
                       ("rust-if-addrs" ,rust-if-addrs-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-multimap" ,rust-multimap-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/libmdns")
    (synopsis
     "mDNS Responder library for building discoverable LAN services in Rust")
    (description
     "This package provides @code{mDNS} Responder library for building discoverable LAN services in Rust.")
    (license license:expat)))

(define-public rust-librespot-discovery-0.6
  (package
    (name "rust-librespot-discovery")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-discovery" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a7yjx1xq9lalvrb5k3z2lqwnd58wg005483nswn4mbpik955acl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-dns-sd" ,rust-dns-sd-0.1)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.10)
                       ("rust-libmdns" ,rust-libmdns-0.9)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-zbus" ,rust-zbus-4))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery logic for librespot")
    (description "This package provides The discovery logic for librespot.")
    (license license:expat)))

(define-public rust-sdl2-sys-0.37
  (package
    (name "rust-sdl2-sys")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sdl2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lr16nj54d77llbbidhnxflg149wm42hsywb0v3dk3phgarfl7cm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2)
                       ("rust-version-compare" ,rust-version-compare-0.1))))
    (home-page "https://github.com/rust-sdl2/rust-sdl2")
    (synopsis "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (description
     "This package provides Raw SDL2 bindings for Rust, used internally rust-sdl2.")
    (license (list license:expat license:zlib))))

(define-public rust-sdl2-0.37
  (package
    (name "rust-sdl2")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sdl2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "106kkjd71kgj9lxmyq43fnjr07f1dynx96vj774dc6jds6kqsj9v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-c-vec" ,rust-c-vec-2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-sdl2-sys" ,rust-sdl2-sys-0.37))))
    (home-page "https://github.com/Rust-SDL2/rust-sdl2")
    (synopsis "SDL2 bindings for Rust")
    (description "This package provides SDL2 bindings for Rust.")
    (license license:expat)))

(define-public rust-extended-0.1
  (package
    (name "rust-extended")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "extended" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r830ak1a9775i9yl5lljm29zbnlncw7xlfz35mhgjrz43c775mg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/depp/extended-rs")
    (synopsis "Extended precision 80-bit floating-point numbers (f80).")
    (description
     "This package provides Extended precision 80-bit floating-point numbers (f80).")
    (license license:expat)))

(define-public rust-symphonia-format-riff-0.5
  (package
    (name "rust-symphonia-format-riff")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-riff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l2zs6zl7q15jhsk9j1lahs2j29k5kkcn5bi9dzr6bwn5wivxxq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-extended" ,rust-extended-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust RIFF demuxer (a part of project Symphonia)")
    (description
     "This package provides Pure Rust RIFF demuxer (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-ogg-0.5
  (package
    (name "rust-symphonia-format-ogg")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-ogg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cd9py2xgx211qvwl9sw8n5l5vgd55vwcmqizh0cyssii5bm18xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust OGG demuxer (a part of project Symphonia)")
    (description
     "This package provides Pure Rust OGG demuxer (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-mkv-0.5
  (package
    (name "rust-symphonia-format-mkv")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-mkv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vrxzr95d1xk2l5jarp7k2935s5ybsyrawwkr4nqixq0l5qk9d0v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust MKV/WebM demuxer (a part of project Symphonia)")
    (description
     "This package provides Pure Rust MKV/@code{WebM} demuxer (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-isomp4-0.5
  (package
    (name "rust-symphonia-format-isomp4")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-isomp4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i68dnhp3q7hv4i51hryw0c75i4l3fx85ffrwphhrrcpsrwg3zdb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust ISO/MP4 demuxer (a part of project Symphonia)")
    (description
     "This package provides Pure Rust ISO/MP4 demuxer (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-caf-0.5
  (package
    (name "rust-symphonia-format-caf")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-caf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l5gjy8asdcw8p2k9xqw0hc8npcz0wrv2wgy55d2k253jv39jg74"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust CAF demuxer (a part of project Symphonia)")
    (description
     "This package provides Pure Rust CAF demuxer (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-vorbis-0.5
  (package
    (name "rust-symphonia-codec-vorbis")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-vorbis" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4z98b8yg2kws3pknw7ipvvca911j3y5xq7n0r6f2kanigpd62s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust Vorbis decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust Vorbis decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-pcm-0.5
  (package
    (name "rust-symphonia-codec-pcm")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-pcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16zq2s8zf0rs6070y3sfyscvm9z1riqvxcbv9plcbsy2axqad5gk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust PCM audio decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust PCM audio decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-alac-0.5
  (package
    (name "rust-symphonia-codec-alac")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-alac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wrq1s6w029bz7lqj08q87i375wvzl78nsj70qll224scik6d2id"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust ALAC decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust ALAC decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-adpcm-0.5
  (package
    (name "rust-symphonia-codec-adpcm")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-adpcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03va885srhrzfz31jvxh2rgr9crnmmlvxmbkx4bdcz1jqgm1ykn9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust ADPCM audio decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust ADPCM audio decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-aac-0.5
  (package
    (name "rust-symphonia-codec-aac")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-aac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w1ga9c7m5bb11rc9bpnjb5g9bqms4x69slix3ikw3dd8nsjbgyd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust AAC decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust AAC decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-bundle-mp3-0.5
  (package
    (name "rust-symphonia-bundle-mp3")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-bundle-mp3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m062zkxq2cbwqxbm3qp4qvgpc9hm49g23vgdc4zpwghf2p2l760"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis
     "Pure Rust MP1, MP2, and MP3 demuxer and decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust MP1, MP2, and MP3 demuxer and decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-utils-xiph-0.5
  (package
    (name "rust-symphonia-utils-xiph")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-utils-xiph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhhs1p0h6wdcgcwfqpmqq07n8v2wvn50razvapr36d41xc74i28"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Project Symphonia utilities for Xiph codecs and formats")
    (description
     "This package provides Project Symphonia utilities for Xiph codecs and formats.")
    (license license:mpl2.0)))

(define-public rust-symphonia-metadata-0.5
  (package
    (name "rust-symphonia-metadata")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g02lhhyf6yyxm7bynx5b9fn2ha39y8fp6cfn72qj05186c2nqmw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Project Symphonia multimedia tag and metadata readers")
    (description
     "This package provides Project Symphonia multimedia tag and metadata readers.")
    (license license:mpl2.0)))

(define-public rust-transpose-0.2
  (package
    (name "rust-transpose")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "transpose" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zp74v7jrjg4jr654dncdj6hqvacicsywyhc62jawgxwhvnimmhs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-strength-reduce" ,rust-strength-reduce-0.2))))
    (home-page "https://github.com/ejmahler/transpose")
    (synopsis "Utility for transposing multi-dimensional data")
    (description
     "This package provides Utility for transposing multi-dimensional data.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strength-reduce-0.2
  (package
    (name "rust-strength-reduce")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strength_reduce" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10jdq9dijjdkb20wg1dmwg447rnj37jbq0mwvbadvqi2gys5x2gy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "http://github.com/ejmahler/strength_reduce")
    (synopsis "Faster integer division and modulus operations")
    (description
     "This package provides Faster integer division and modulus operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-primal-check-0.3
  (package
    (name "rust-primal-check")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "primal-check" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "025xnak4rhkwa4h970bjb3cvp2k853wviyr84n8gjfhy65dqj3fw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-integer" ,rust-num-integer-0.1))))
    (home-page "https://github.com/huonw/primal")
    (synopsis "Fast standalone primality testing.")
    (description "This package provides Fast standalone primality testing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustfft-6
  (package
    (name "rust-rustfft")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustfft" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11hx83yr2h2jszkba9qhq2d08q9i5rsashq62rfhqvahpihnb023"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-complex" ,rust-num-complex-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-primal-check" ,rust-primal-check-0.3)
                       ("rust-strength-reduce" ,rust-strength-reduce-0.2)
                       ("rust-transpose" ,rust-transpose-0.2)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/ejmahler/RustFFT")
    (synopsis "High-performance FFT library written in pure Rust")
    (description
     "This package provides High-performance FFT library written in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-symphonia-core-0.5
  (package
    (name "rust-symphonia-core")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hr2w2a217vq4lpghszmsdwxr5ilh5d1ysfm3cixbirxkrvhd0vr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustfft" ,rust-rustfft-6))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Project Symphonia shared structs, traits, and features")
    (description
     "This package provides Project Symphonia shared structs, traits, and features.")
    (license license:mpl2.0)))

(define-public rust-symphonia-bundle-flac-0.5
  (package
    (name "rust-symphonia-bundle-flac")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-bundle-flac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15xxncx6gfh7jwvxvqqw4f8x9ic4bfzpyv3s77a0hwwa54s4zqvj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis
     "Pure Rust FLAC demuxer and decoder (a part of project Symphonia)")
    (description
     "This package provides Pure Rust FLAC demuxer and decoder (a part of project Symphonia).")
    (license license:mpl2.0)))

(define-public rust-symphonia-0.5
  (package
    (name "rust-symphonia")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1agmsnmzgsmvd70hq760nvkjrb52nnjmz5hgn1xp6x7fwwm98p41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-symphonia-bundle-flac" ,rust-symphonia-bundle-flac-0.5)
                       ("rust-symphonia-bundle-mp3" ,rust-symphonia-bundle-mp3-0.5)
                       ("rust-symphonia-codec-aac" ,rust-symphonia-codec-aac-0.5)
                       ("rust-symphonia-codec-adpcm" ,rust-symphonia-codec-adpcm-0.5)
                       ("rust-symphonia-codec-alac" ,rust-symphonia-codec-alac-0.5)
                       ("rust-symphonia-codec-pcm" ,rust-symphonia-codec-pcm-0.5)
                       ("rust-symphonia-codec-vorbis" ,rust-symphonia-codec-vorbis-0.5)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-symphonia-format-caf" ,rust-symphonia-format-caf-0.5)
                       ("rust-symphonia-format-isomp4" ,rust-symphonia-format-isomp4-0.5)
                       ("rust-symphonia-format-mkv" ,rust-symphonia-format-mkv-0.5)
                       ("rust-symphonia-format-ogg" ,rust-symphonia-format-ogg-0.5)
                       ("rust-symphonia-format-riff" ,rust-symphonia-format-riff-0.5)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.5))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust media container and audio decoding library")
    (description
     "This package provides Pure Rust media container and audio decoding library.")
    (license license:mpl2.0)))

(define-public rust-slice-ring-buffer-0.3
  (package
    (name "rust-slice-ring-buffer")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slice-ring-buffer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mi7iigijbcp0c9m2sslfny5q2savysgv1grg67kdch9v8mk3bl4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/liquidityc/slice_ring_buffer")
    (synopsis "double-ended queue that Deref's into a slice.")
    (description
     "This package provides a double-ended queue that Deref's into a slice.")
    (license (list license:expat license:asl2.0))))

(define-public rust-minimp3-fixed-0.5
  (package
    (name "rust-minimp3-fixed")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "minimp3_fixed" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06wmic6gy3k3jmbz767clapx98v20aqmc9kc76p9gnkmgr7g3c22"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-minimp3-sys" ,rust-minimp3-sys-0.3)
                       ("rust-slice-ring-buffer" ,rust-slice-ring-buffer-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/BOB450/minimp3-rs.git")
    (synopsis
     "Rust bindings for the minimp3 library. With Security patch applied")
    (description
     "This package provides Rust bindings for the minimp3 library.  With Security patch applied.")
    (license license:expat)))

(define-public rust-rodio-0.19
  (package
    (name "rust-rodio")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rodio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jvs8a6iq7h7s23acq1d76jk9zlc85snap58sgrkg3d3q4ksc1k0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-claxon" ,rust-claxon-0.4)
                       ("rust-cpal" ,rust-cpal-0.15)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-hound" ,rust-hound-3)
                       ("rust-lewton" ,rust-lewton-0.10)
                       ("rust-minimp3-fixed" ,rust-minimp3-fixed-0.5)
                       ("rust-symphonia" ,rust-symphonia-0.5)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Audio playback library")
    (description "This package provides Audio playback library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ogg-0.9
  (package
    (name "rust-ogg")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ogg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wbc72zfdz3k54fpar88n9lyyaavhfncmiyy46x3018m71k02xsl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.6))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description
     "This package provides Ogg container decoder and encoder written in pure Rust.")
    (license license:bsd-3)))

(define-public rust-librespot-metadata-0.6
  (package
    (name "rust-librespot-metadata")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pxyz32fklj5cmsl3603w6x4nsdab7lxrsv2w39is6s0db2dbxkw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata logic for librespot")
    (description "This package provides The metadata logic for librespot.")
    (license license:expat)))

(define-public rust-gstreamer-audio-sys-0.23
  (package
    (name "rust-gstreamer-audio-sys")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0br68dwkffkgbnk47jsrzzjsnk4xayw4r14sfi2rmvsi1vca6w7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.23)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.23)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstaudio-1.0")
    (description "This package provides FFI bindings to libgstaudio-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-audio-0.23
  (package
    (name "rust-gstreamer-audio")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18nx5h7fjwyi944k2s9ybbk7hdyn3d86m5c922mjr54wbjdh19jj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gstreamer" ,rust-gstreamer-0.23)
                       ("rust-gstreamer-audio-sys" ,rust-gstreamer-audio-sys-0.23)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.23)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Audio library")
    (description
     "This package provides Rust bindings for GStreamer Audio library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-base-0.23
  (package
    (name "rust-gstreamer-base")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16w90pn4rfhg8pjsaqgvfls0cj35502kirb4rx85k3grhdwxnlni"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic-refcell" ,rust-atomic-refcell-0.1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gstreamer" ,rust-gstreamer-0.23)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.23)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base library")
    (description
     "This package provides Rust bindings for GStreamer Base library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-base-sys-0.23
  (package
    (name "rust-gstreamer-base-sys")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-base-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "073rn88smdjpzxfw21b8f0ml604nxjzcpnp556qmwg9abz8w4z6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.23)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstbase-1.0")
    (description "This package provides FFI bindings to libgstbase-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-app-sys-0.23
  (package
    (name "rust-gstreamer-app-sys")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-app-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mjfdd9zicig2w2r35v98jmxlq8z967i2w8ny939lqlw2hmmq2kb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.23)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.23)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstapp-1.0")
    (description "This package provides FFI bindings to libgstapp-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-app-0.23
  (package
    (name "rust-gstreamer-app")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-app" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h26g1vwxacbi4jkhlmwdyw1mgmcsnysz0whhq051kwh26hbvds1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gstreamer" ,rust-gstreamer-0.23)
                       ("rust-gstreamer-app-sys" ,rust-gstreamer-app-sys-0.23)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.23)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer App library")
    (description
     "This package provides Rust bindings for GStreamer App library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gstreamer-sys-0.23
  (package
    (name "rust-gstreamer-sys")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06cp8rbyydhz9f6b39b4yd44yjrv0mv5ns1wrrk71ak9m3h1mkqn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.20)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.20)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-7))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstreamer-1.0")
    (description "This package provides FFI bindings to libgstreamer-1.0.")
    (license license:expat)))

(define-public rust-gstreamer-0.23
  (package
    (name "rust-gstreamer")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gstreamer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15axrkh09p60y4fdfk8bwhbj6q0il4182wpbhm7l5nkdx2rb233h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.23)
                       ("rust-itertools" ,rust-itertools-0.13)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-muldiv" ,rust-muldiv-1)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-rational" ,rust-num-rational-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-option-operations" ,rust-option-operations-0.5)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer")
    (description "This package provides Rust bindings for GStreamer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-interface-0.53
  (package
    (name "rust-windows-interface")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-interface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q4bb5zigzr3083kwb7qkhx63dlymwx8gy6mw7jgm25281qmacys"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The interface macro for the windows crate")
    (description
     "This package provides The interface macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-implement-0.53
  (package
    (name "rust-windows-implement")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-implement" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gd05sw9knn8i7n9ip1kdwpxqcwmldja3w32m16chjcjprkc4all"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "The implement macro for the windows crate")
    (description
     "This package provides The implement macro for the windows crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-core-0.54
  (package
    (name "rust-windows-core")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r8x2sgl4qq1h23ldf4z7cj213k0bz7479m8a156h79mi6f1nrhj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-result" ,rust-windows-result-0.1)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-windows-0.54
  (package
    (name "rust-windows")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "windows" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j8vd8sg2rbln6g3a608qg1a7r2lwxcga78mmxjjin5ybmrfallj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-windows-core" ,rust-windows-core-0.54)
                       ("rust-windows-implement" ,rust-windows-implement-0.53)
                       ("rust-windows-interface" ,rust-windows-interface-0.53)
                       ("rust-windows-targets" ,rust-windows-targets-0.52))))
    (home-page "https://github.com/microsoft/windows-rs")
    (synopsis "Rust for Windows")
    (description "This package provides Rust for Windows.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oboe-sys-0.6
  (package
    (name "rust-oboe-sys")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oboe-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17g7yb4kk6bakc4rhv1izfcqjgqhpkasgq6gf20nc79b9adb12vc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-fetch-unroll" ,rust-fetch-unroll-0.3))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Unsafe bindings for oboe an android library for low latency audio IO")
    (description
     "This package provides Unsafe bindings for oboe an android library for low latency audio IO.")
    (license license:asl2.0)))

(define-public rust-oboe-0.6
  (package
    (name "rust-oboe")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oboe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yv7x06mwk61nsy3ckcmqwgg9q0n3j4y4zncz3sl6pcyskmipdp8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-jni" ,rust-jni-0.21)
                       ("rust-ndk" ,rust-ndk-0.8)
                       ("rust-ndk-context" ,rust-ndk-context-0.1)
                       ("rust-num-derive" ,rust-num-derive-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oboe-sys" ,rust-oboe-sys-0.6))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Safe interface for oboe an android library for low latency audio IO")
    (description
     "This package provides Safe interface for oboe an android library for low latency audio IO.")
    (license license:asl2.0)))

(define-public rust-jack-sys-0.5
  (package
    (name "rust-jack-sys")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aw6zishflmd5v9dz5yvpc5f9jsfm9pjjhzvdmbjp8lmkdhvf4v0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.7)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API")
    (description
     "This package provides Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-0.11
  (package
    (name "rust-jack")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kd6p6bfmxyclkkq9pkrqyynf0mj53ias4binx7kbyxfqaiihnhf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-jack-sys" ,rust-jack-sys-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK")
    (description "This package provides Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-dasp-sample-0.11
  (package
    (name "rust-dasp-sample")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dasp_sample" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zzw35akm3qs2rixbmlijk6h0l4g9ry6g74qc59zv1q8vs1f31qc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustaudio/sample")
    (synopsis
     "An abstraction for audio PCM DSP samples, along with useful conversions and operations")
    (description
     "This package provides An abstraction for audio PCM DSP samples, along with useful conversions and
operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-coreaudio-rs-0.11
  (package
    (name "rust-coreaudio-rs")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "coreaudio-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kmssby4rqhv2iq1a8zmaav5p3bl40qs0wah9zv65ikr5lbpf41j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "friendly rust interface for Apple's CoreAudio API.")
    (description
     "This package provides a friendly rust interface for Apple's @code{CoreAudio}
API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cpal-0.15
  (package
    (name "rust-cpal")
    (version "0.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yd7d51kcf8ml0bfkjrac12zgfjzk21wa97maxg0fhzpr03sngc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa" ,rust-alsa-0.9)
                       ("rust-asio-sys" ,rust-asio-sys-0.2)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.11)
                       ("rust-dasp-sample" ,rust-dasp-sample-0.11)
                       ("rust-jack" ,rust-jack-0.11)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-ndk" ,rust-ndk-0.8)
                       ("rust-ndk-context" ,rust-ndk-context-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oboe" ,rust-oboe-0.6)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-windows" ,rust-windows-0.54))))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust")
    (description
     "This package provides Low-level cross-platform audio I/O library in pure Rust.")
    (license license:asl2.0)))

(define-public rust-alsa-0.9
  (package
    (name "rust-alsa")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hvxc447bsynyhzhmznw6w2kwbid83p712dls4h1x8w3pavp4xgd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description
     "This package provides Thin but safe wrappers for ALSA (Linux sound API).")
    (license (list license:asl2.0 license:expat))))

(define-public rust-librespot-playback-0.6
  (package
    (name "rust-librespot-playback")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-playback" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r6zf72kyhlzlq7nsv7c56kmwqgj6kq4v3bz81sr532cl1vgglaf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa" ,rust-alsa-0.9)
                       ("rust-cpal" ,rust-cpal-0.15)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-glib" ,rust-glib-0.20)
                       ("rust-gstreamer" ,rust-gstreamer-0.23)
                       ("rust-gstreamer-app" ,rust-gstreamer-app-0.23)
                       ("rust-gstreamer-audio" ,rust-gstreamer-audio-0.23)
                       ("rust-jack" ,rust-jack-0.11)
                       ("rust-libpulse-binding" ,rust-libpulse-binding-2)
                       ("rust-libpulse-simple-binding" ,rust-libpulse-simple-binding-2)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.6)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-librespot-metadata" ,rust-librespot-metadata-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ogg" ,rust-ogg-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-portaudio-rs" ,rust-portaudio-rs-0.3)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-distr" ,rust-rand-distr-0.4)
                       ("rust-rodio" ,rust-rodio-0.19)
                       ("rust-sdl2" ,rust-sdl2-0.37)
                       ("rust-shell-words" ,rust-shell-words-1)
                       ("rust-symphonia" ,rust-symphonia-0.5)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio playback logic for librespot")
    (description
     "This package provides The audio playback logic for librespot.")
    (license license:expat)))

(define-public rust-librespot-connect-0.6
  (package
    (name "rust-librespot-connect")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q7s2visnzxk5ddgsigh3l8k2j27g91zbkr5li8y17366ib3mkk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.6)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-3)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery and Spotify Connect logic for librespot")
    (description
     "This package provides The discovery and Spotify Connect logic for librespot.")
    (license license:expat)))

(define-public rust-vergen-lib-0.1
  (package
    (name "rust-vergen-lib")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vergen-lib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ix19k2kvn4hzhhl6nld9h7fh1qh5z7j51z5rn2zq28wfpk6giy0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-rustversion" ,rust-rustversion-1))))
    (home-page "https://github.com/rustyhorde/vergen")
    (synopsis "Common code used to support the vergen libraries")
    (description
     "This package provides Common code used to support the vergen libraries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.169")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
     "This package provides Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sysinfo-0.32
  (package
    (name "rust-sysinfo")
    (version "0.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sysinfo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bzlj3afjz4ibdsfchjk1f4md6djffw668f3npiykwph38jcscsc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-ntapi" ,rust-ntapi-0.4)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows" ,rust-windows-0.57))))
    (home-page "https://github.com/GuillaumeGomez/sysinfo")
    (synopsis
     "Library to get system information such as processes, CPUs, disks, components and networks")
    (description
     "This package provides Library to get system information such as processes, CPUs, disks, components and
networks.")
    (license license:expat)))

(define-public rust-rustc-version-0.4
  (package
    (name "rust-rustc-version")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc_version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-semver" ,rust-semver-1))))
    (home-page "https://github.com/djc/rustc-version-rs")
    (synopsis "library for querying the version of a installed rustc compiler")
    (description
     "This package provides a library for querying the version of a installed rustc
compiler.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-automata" ,rust-regex-automata-0.4)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
     "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (description
     "This package provides An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-2
  (package
    (name "rust-thiserror-impl")
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m77z5vb4w7xn7y12zxnbwncva4bwbi45y45xvkf5aki20kzll3v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
     "This package provides Implementation detail of the `thiserror` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-2
  (package
    (name "rust-thiserror")
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k5j0ri0kjrnlblv5ikaglbkg1sxxwh0qrxbidxgc38rs0zn8wph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "This package provides derive(Error).")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-metadata-0.19
  (package
    (name "rust-cargo-metadata")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo_metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "096r6ai6b8r6z42lasr16pd0zbgndvs7n6a3mwh636axmmm70sc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-camino" ,rust-camino-1)
                       ("rust-cargo-platform" ,rust-cargo-platform-0.1)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-2))))
    (home-page "https://github.com/oli-obk/cargo_metadata")
    (synopsis "structured access to the output of `cargo metadata`")
    (description
     "This package provides structured access to the output of `cargo metadata`.")
    (license license:expat)))

(define-public rust-vergen-9
  (package
    (name "rust-vergen")
    (version "9.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vergen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f86dz7mm0blfw17lggkkwm548mdjgq8f7llqxaz8pghz345zwii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cargo-metadata" ,rust-cargo-metadata-0.19)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-sysinfo" ,rust-sysinfo-0.32)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-vergen-lib" ,rust-vergen-lib-0.1))))
    (home-page "https://github.com/rustyhorde/vergen")
    (synopsis
     "Generate 'cargo:rustc-env' instructions via 'build.rs' for use in your code via the 'env!' macro")
    (description
     "This package provides Generate cargo:rustc-env instructions via build.rs for use in your code via the
env! macro.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustversion-1
  (package
    (name "rust-rustversion")
    (version "1.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustversion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m39qd65jcd1xgqzdm3017ppimiggh2446xngwp1ngr8hjbmpi7p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/rustversion")
    (synopsis "Conditional compilation according to rustc compiler version")
    (description
     "This package provides Conditional compilation according to rustc compiler version.")
    (license (list license:expat license:asl2.0))))

(define-public rust-darling-macro-0.20
  (package
    (name "rust-darling-macro")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling-core" ,rust-darling-core-0.20)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
     "This package provides Internal support for a proc-macro library for reading attributes into structs
when implementing custom derives.  Use https://crates.io/crates/darling in your
code.")
    (license license:expat)))

(define-public rust-strsim-0.11
  (package
    (name "rust-strsim")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strsim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis
     "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (description
     "This package provides Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (license license:expat)))

(define-public rust-darling-core-0.20
  (package
    (name "rust-darling-core")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fnv" ,rust-fnv-1)
                       ("rust-ident-case" ,rust-ident-case-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-strsim" ,rust-strsim-0.11)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
     "This package provides Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.")
    (license license:expat)))

(define-public rust-darling-0.20
  (package
    (name "rust-darling")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling-core" ,rust-darling-core-0.20)
                       ("rust-darling-macro" ,rust-darling-macro-0.20))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis "proc-macro library for reading attributes into structs when
implementing custom derives.")
    (description
     "This package provides a proc-macro library for reading attributes into structs
when implementing custom derives.")
    (license license:expat)))

(define-public rust-derive-builder-core-0.20
  (package
    (name "rust-derive-builder-core")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s640r6q46c2iiz25sgvxw3lk6b6v5y8hwylng7kas2d09xwynrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-darling" ,rust-darling-0.20)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis "Internal helper library for the derive_builder crate")
    (description
     "This package provides Internal helper library for the derive_builder crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-macro-0.20
  (package
    (name "rust-derive-builder-macro")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g1zznpqrmvjlp2w7p0jzsjvpmw5rvdag0rfyypjhnadpzib0qxb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-builder-core" ,rust-derive-builder-core-0.20)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs")
    (description
     "This package provides Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-0.20
  (package
    (name "rust-derive-builder")
    (version "0.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0is9z7v3kznziqsxa5jqji3ja6ay9wzravppzhcaczwbx84znzah"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-builder-macro" ,rust-derive-builder-macro-0.20))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs")
    (description
     "This package provides Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anyhow-1
  (package
    (name "rust-anyhow")
    (version "1.0.95")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anyhow" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "010vd1ki8w84dzgx6c81sc8qm9n02fxic1gkpv52zp4nwrn0kb1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3))))
    (home-page "https://github.com/dtolnay/anyhow")
    (synopsis "Flexible concrete Error type built on std::error::Error")
    (description
     "This package provides Flexible concrete Error type built on std::error::Error.")
    (license (list license:expat license:asl2.0))))

(define-public rust-vergen-gitcl-1
  (package
    (name "rust-vergen-gitcl")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vergen-gitcl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15xrh1h6mj3nc6c34hpdzcy82sk7bq2mx6lylq7b12pr15nh09q2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-vergen" ,rust-vergen-9)
                       ("rust-vergen-lib" ,rust-vergen-lib-0.1))))
    (home-page "https://github.com/rustyhorde/vergen")
    (synopsis
     "Generate 'cargo:rustc-env' instructions via 'build.rs' for use in your code via the 'env!' macro")
    (description
     "This package provides Generate cargo:rustc-env instructions via build.rs for use in your code via the
env! macro.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tungstenite-0.24
  (package
    (name "rust-tungstenite")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nsxnxazk4nisgsqpywi6za0hsbc2rd15r1scb5pag7dqvbir8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-http" ,rust-http-1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Lightweight stream-based @code{WebSocket} implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-tungstenite-0.24
  (package
    (name "rust-tokio-tungstenite")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nfw1i6yy120a14h1xagd4f31k3g1mz4rdxpvgh77jcd4i7ggigd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.8)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tungstenite" ,rust-tungstenite-0.24)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Tokio binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-priority-queue-2
  (package
    (name "rust-priority-queue")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "priority-queue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g8qr3s9wx9id50h20ma0mkqls9hz2wnmz1zg1iqmj3v57dpak3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/garro95/priority-queue")
    (synopsis
     "Priority Queue implemented as a heap with a function to efficiently change the priority of an item.")
    (description
     "This package provides a Priority Queue implemented as a heap with a function to
efficiently change the priority of an item.")
    (license (list license:lgpl3+ license:mpl2.0))))

(define-public rust-protobuf-parse-3
  (package
    (name "rust-protobuf-parse")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "protobuf-parse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mv27iwcrli8vvy89ivji59cdizsxcry1d24hiqmbd7a6ghk08rj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-3)
                       ("rust-protobuf-support" ,rust-protobuf-support-3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-which" ,rust-which-4))))
    (home-page
     "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-parse/")
    (synopsis
     "Parse `.proto` files.

Files are parsed into a `protobuf::descriptor::FileDescriptorSet` object using either:
* pure rust parser (no dependencies)
* `protoc` binary (more reliable and compatible with Google's implementation)")
    (description
     "This package provides Parse `.proto` files.  Files are parsed into a
`protobuf::descriptor::@code{FileDescriptorSet`} object using either: * pure
rust parser (no dependencies) * `protoc` binary (more reliable and compatible
with Google's implementation).")
    (license license:expat)))

(define-public rust-protobuf-codegen-3
  (package
    (name "rust-protobuf-codegen")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "protobuf-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i41q436d6azdc5g3b43zp93z9dan9303nxi0h7a6sa72hzq6sz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-protobuf" ,rust-protobuf-3)
                       ("rust-protobuf-parse" ,rust-protobuf-parse-3)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "")
    (synopsis "Code generator for Rust Protocol Buffers bindings")
    (description
     "This package provides Code generator for Rust Protocol Buffers bindings.")
    (license license:expat)))

(define-public rust-protobuf-support-3
  (package
    (name "rust-protobuf-support")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "protobuf-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lijcs3agmks766pihqm01in552p92pnyfw403m7ba1qp4hgv25h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
     "Code supporting protobuf implementation. None of code in this crate is public API.")
    (description
     "This package provides Code supporting protobuf implementation.  None of code in this crate is public
API.")
    (license license:expat)))

(define-public rust-protobuf-3
  (package
    (name "rust-protobuf")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "protobuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wlg1k6xs8k25shvdryzlz5fhx4h2x6c290ik26innzpkd6wd9x3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-protobuf-support" ,rust-protobuf-support-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "")
    (synopsis "Protocol Buffers - Google's data interchange format")
    (description
     "This package provides Protocol Buffers - Google's data interchange format.")
    (license license:expat)))

(define-public rust-librespot-protocol-0.6
  (package
    (name "rust-librespot-protocol")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ih5ignz7srpl3zabq22p7vzn9x6hfjarrjlfhas5cx1nm92z040"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-3)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-3))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description
     "This package provides The protobuf logic for communicating with Spotify servers.")
    (license license:expat)))

(define-public rust-oauth2-4
  (package
    (name "rust-oauth2")
    (version "4.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oauth2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwkmwxwygl4fwghgyanixzqgn7yvkwwwacdghz7x124v36l3263"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/ramosbugs/oauth2-rs")
    (synopsis "An extensible, strongly-typed implementation of OAuth2")
    (description
     "This package provides An extensible, strongly-typed implementation of OAuth2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-oauth-0.6
  (package
    (name "rust-librespot-oauth")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-oauth" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kyiz3a7nn634sq0ap56737myp1sm06a9qlkwi1b9hx6k9vsvp00"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-oauth2" ,rust-oauth2-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis
     "OAuth authorization code flow with PKCE for obtaining a Spotify access token")
    (description
     "This package provides OAuth authorization code flow with PKCE for obtaining a Spotify access token.")
    (license license:expat)))

(define-public rust-tokio-rustls-0.26
  (package
    (name "rust-tokio-rustls")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dxz4bhkn4bwnvzjqvqlg70ba5fslnmf9r6yr87wzq5cx9shjvaz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls" ,rust-rustls-0.23)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/tokio-rustls")
    (synopsis "Asynchronous TLS/SSL streams for Tokio using Rustls")
    (description
     "This package provides Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-webpki-root-certs-0.26
  (package
    (name "rust-webpki-root-certs")
    (version "0.26.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-root-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p15xwdlibwqlmkqjb6qqikypyxqb0lwxf70rxa01wzipm4xmmcw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustls-pki-types" ,rust-rustls-pki-types-1))))
    (home-page "https://github.com/rustls/webpki-roots")
    (synopsis
     "Mozilla trusted certificate authorities in self-signed X.509 format for use with crates other than webpki")
    (description
     "This package provides Mozilla trusted certificate authorities in self-signed X.509 format for use with
crates other than webpki.")
    (license license:mpl2.0)))

(define-public rust-rustls-platform-verifier-0.5
  (package
    (name "rust-rustls-platform-verifier")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-platform-verifier" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nlrxbw2i5q7dxz2gnif14hz5ph67b5d8viqr4r06yd18icc84p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-logger" ,rust-android-logger-0.13)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-core-foundation" ,rust-core-foundation-0.10)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.8)
                       ("rust-rustls-platform-verifier-android" ,rust-rustls-platform-verifier-android-0.1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-security-framework" ,rust-security-framework-3)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2)
                       ("rust-webpki-root-certs" ,rust-webpki-root-certs-0.26)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/rustls/rustls-platform-verifier")
    (synopsis
     "rustls-platform-verifier supports verifying TLS certificates in rustls with the operating system verifier")
    (description
     "This package provides rustls-platform-verifier supports verifying TLS certificates in rustls with the
operating system verifier.")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-sys-2
  (package
    (name "rust-security-framework-sys")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mbhagj98y2byhjkr353y1nings01pfa9yk0gxmcb0ydd0vzsqqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
     "This package provides Apple `Security.framework` low-level FFI bindings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-3
  (package
    (name "rust-security-framework")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g1wq04rb6gsyfawphv5vhmmicbm5l25gsvr05mvng6cpz4zilw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-core-foundation" ,rust-core-foundation-0.10)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description
     "This package provides Security.framework bindings for @code{macOS} and @code{iOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-native-certs-0.8
  (package
    (name "rust-rustls-native-certs")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ls7laa3748mkn23fmi3g4mlwk131lx6chq2lyc8v2mmabfz5kvz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-security-framework" ,rust-security-framework-3))))
    (home-page "https://github.com/rustls/rustls-native-certs")
    (synopsis
     "rustls-native-certs allows rustls to use the platform native certificate store")
    (description
     "This package provides rustls-native-certs allows rustls to use the platform native certificate store.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-hyper-rustls-0.27
  (package
    (name "rust-hyper-rustls")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.8)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-platform-verifier" ,rust-rustls-platform-verifier-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description
     "This package provides Rustls+hyper integration for pure rust HTTPS.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-tokio-openssl-0.6
  (package
    (name "rust-tokio-openssl")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-openssl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pga4xm5fcms6k1rqg4hsl8mmna7qiizhdlsgxbbffx4r94nipsr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/tokio-rs/tokio-openssl")
    (synopsis "An implementation of SSL streams for Tokio backed by OpenSSL")
    (description
     "This package provides An implementation of SSL streams for Tokio backed by @code{OpenSSL}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytes-1
  (package
    (name "rust-bytes")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16ykzx24v1x4f42v2lxyvlczqhdfji3v7r4ghwckpwijzvb1hn9j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis "Types and traits for working with bytes")
    (description
     "This package provides Types and traits for working with bytes.")
    (license license:expat)))

(define-public rust-hyper-proxy2-0.1
  (package
    (name "rust-hyper-proxy2")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-proxy2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1brzvj6v5rfzq17x6wbg41vcqhpwli87phhlf0f4mg5h7yrbfhwh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-headers" ,rust-headers-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.6)
                       ("rust-hyper-util" ,rust-hyper-util-0.10)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/siketyan/hyper-proxy2")
    (synopsis "proxy connector for Hyper-based applications")
    (description
     "This package provides a proxy connector for Hyper-based applications.")
    (license license:expat)))

(define-public rust-spinning-top-0.3
  (package
    (name "rust-spinning-top")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spinning_top" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "001kjbiz1gg111rsqxc4pq9a1izx7wshkk38f69h1dbgf4fjsvfr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4))))
    (home-page "https://github.com/rust-osdev/spinning_top")
    (synopsis
     "simple spinlock crate based on the abstractions provided by `lock_api`.")
    (description
     "This package provides a simple spinlock crate based on the abstractions provided
by `lock_api`.")
    (license (list license:expat license:asl2.0))))

(define-public rust-prost-types-0.11
  (package
    (name "rust-prost-types")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ryk38sqkp2nf4dgdqdfbgn6zwwvjraw6hqq6d9a6088shj4di1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-prost" ,rust-prost-0.11))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "Prost definitions of Protocol Buffers well known types")
    (description
     "This package provides Prost definitions of Protocol Buffers well known types.")
    (license license:asl2.0)))

(define-public rust-quanta-0.12
  (package
    (name "rust-quanta")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quanta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rngcl6ar7v5n8442dxxcpry28z2jkz6ylz31gr7xg5r1f6ycg3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-raw-cpuid" ,rust-raw-cpuid-11)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description "This package provides high-speed timing library.")
    (license license:expat)))

(define-public rust-nonzero-ext-0.3
  (package
    (name "rust-nonzero-ext")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nonzero_ext" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08fghyinb07xwhbj7vwvlhg45g5cvhvld2min25njidir12rdgrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/antifuchs/nonzero_ext")
    (synopsis "Extensions and additional traits for non-zero integer types")
    (description
     "This package provides Extensions and additional traits for non-zero integer types.")
    (license license:asl2.0)))

(define-public rust-governor-0.6
  (package
    (name "rust-governor")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "governor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yw66yb1rfc7np23n9af9sb8kbhv3jnhvg3an1rsydbbxr1gb9v8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-futures-timer" ,rust-futures-timer-3)
                       ("rust-no-std-compat" ,rust-no-std-compat-0.4)
                       ("rust-nonzero-ext" ,rust-nonzero-ext-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-quanta" ,rust-quanta-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-spinning-top" ,rust-spinning-top-0.3))))
    (home-page "https://github.com/boinkor-net/governor")
    (synopsis "rate-limiting implementation in Rust")
    (description
     "This package provides a rate-limiting implementation in Rust.")
    (license license:expat)))

(define-public rust-librespot-core-0.6
  (package
    (name "rust-librespot-core")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rs4bv62kx3r00d69cyv6bx74k8c8b5200iqz5crm56gxw1n7iy4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-governor" ,rust-governor-0.6)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-proxy2" ,rust-hyper-proxy2-0.1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.27)
                       ("rust-hyper-util" ,rust-hyper-util-0.10)
                       ("rust-librespot-oauth" ,rust-librespot-oauth-0.6)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nonzero-ext" ,rust-nonzero-ext-0.3)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-derive" ,rust-num-derive-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-priority-queue" ,rust-priority-queue-2)
                       ("rust-protobuf" ,rust-protobuf-3)
                       ("rust-quick-xml" ,rust-quick-xml-0.36)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-shannon" ,rust-shannon-0.2)
                       ("rust-sysinfo" ,rust-sysinfo-0.31)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.24)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-vergen-gitcl" ,rust-vergen-gitcl-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The core functionality provided by librespot")
    (description
     "This package provides The core functionality provided by librespot.")
    (license license:expat)))

(define-public rust-hyper-1
  (package
    (name "rust-hyper")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q7akfb443yrjzkmnnbp2vs8zi15hgbk466rr4y144v4ppabhvr5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-httpdate" ,rust-httpdate-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-want" ,rust-want-0.3))))
    (home-page "https://hyper.rs")
    (synopsis "protective and efficient HTTP library for all.")
    (description
     "This package provides a protective and efficient HTTP library for all.")
    (license license:expat)))

(define-public rust-http-body-util-0.1
  (package
    (name "rust-http-body-util")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))))
    (home-page "https://github.com/hyperium/http-body")
    (synopsis "Combinators and adapters for HTTP request or response bodies.")
    (description
     "This package provides Combinators and adapters for HTTP request or response bodies.")
    (license license:expat)))

(define-public rust-librespot-audio-0.6
  (package
    (name "rust-librespot-audio")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f5sd0a68v16irrzk9saj151295mxndxvfv1dj9l4c2mwxpmc1vy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.8)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-ctr" ,rust-ctr-0.9)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.10)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio fetching logic for librespot")
    (description
     "This package provides The audio fetching logic for librespot.")
    (license license:expat)))

(define-public rust-librespot-0.6
  (package
    (name "rust-librespot")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k7ha2mn3zr4z1mnia093hyahq133p63si5q4x9y6rr7ggwv3r4n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.6)
                       ("rust-librespot-connect" ,rust-librespot-connect-0.6)
                       ("rust-librespot-core" ,rust-librespot-core-0.6)
                       ("rust-librespot-discovery" ,rust-librespot-discovery-0.6)
                       ("rust-librespot-metadata" ,rust-librespot-metadata-0.6)
                       ("rust-librespot-oauth" ,rust-librespot-oauth-0.6)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.6)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sysinfo" ,rust-sysinfo-0.31)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (native-inputs (list pkg-config alsa-lib))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis
     "An open source client library for Spotify, with support for Spotify Connect")
    (description
     "This package provides An open source client library for Spotify, with support for Spotify Connect.")
    (license license:expat)))
