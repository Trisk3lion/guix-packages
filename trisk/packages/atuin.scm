(define-module (trisk packages atuin)
  #:use-module (ice-9 string-fun)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages protobuf)
  #:use-module ((guix licenses) #:prefix license:)
  )

;; guix import crate eyre

(define-public rust-eyre-0.6
  (package
    (name "rust-eyre")
    (version "0.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eyre" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (rust-indenter-0.3
                       rust-once-cell-1
                       rust-pyo3-0.20)
       #:cargo-development-inputs (rust-anyhow-1
                                   rust-backtrace-0.3
                                   rust-futures-0.3
                                   rust-pyo3-0.20
                                   rust-rustversion-1
                                   rust-syn-2
                                   rust-thiserror-1
                                   rust-trybuild-1)))
    (home-page "https://github.com/eyre-rs/eyre")
    (synopsis
     "Flexible concrete Error Reporting type built on std::error::Error with customizable Reports")
    (description
     "This package provides Flexible concrete Error Reporting type built on std::error::Error with
customizable Reports.")
    (license (list license:expat license:asl2.0))))

;; guix import crate atuin -r

(define-public rust-tracing-tree-0.3
  (package
    (name "rust-tracing-tree")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gkxh3v346k82v4zzjryzq899y9f5w576j74z8vcncq0r3964v5m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-nu-ansi-term-0.50
                       rust-time-0.3
                       rust-tracing-core-0.1
                       rust-tracing-log-0.2
                       rust-tracing-subscriber-0.3)))
    (home-page "https://github.com/davidbarsky/tracing-tree")
    (synopsis "Tracing Layer which prints a tree of spans and events.")
    (description
     "This package provides a Tracing Layer which prints a tree of spans and events.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix-0.38
  (package
    (name "rust-rustix")
    (version "0.38.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03vkqa2ism7q56rkifyy8mns0wwqrk70f4i4fd53r97p8b05xp3h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-bitflags-2
                       rust-compiler-builtins-0.1
                       rust-errno-0.3
                       rust-itoa-1
                       rust-libc-0.2
                       rust-linux-raw-sys-0.4
                       rust-once-cell-1
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-windows-sys-0.52)))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
     "This package provides Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls.")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-runtime-format-0.1
  (package
    (name "rust-runtime-format")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "runtime-format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-tinyvec-1)))
    (home-page "https://github.com/conradludgate/strfmt")
    (synopsis "rust library for formatting dynamic strings")
    (description
     "This package provides rust library for formatting dynamic strings.")
    (license license:expat)))

(define-public rust-clap-complete-nushell-4
  (package
    (name "rust-clap-complete-nushell")
    (version "4.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_complete_nushell" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03m4wddjklz18havdsi4qy8b4zdsw7zg76lc1xrczg06w0823qsz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-clap-4
                       rust-clap-complete-4)))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "generator library used with clap for Nushell completion scripts")
    (description
     "This package provides a generator library used with clap for Nushell completion
scripts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.5.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1860xq3rbgwsqwcj9rd14cky9iiywwx86j7fvvngdjixbyfka7ah"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-heck-0.5
                       rust-proc-macro2-1
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Parse command line argument by defining a struct, derive crate")
    (description
     "This package provides Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-demangle-0.1
  (package
    (name "rust-rustc-demangle")
    (version "0.1.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc-demangle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-compiler-builtins-0.1
                       rust-rustc-std-workspace-core-1)))
    (home-page "https://github.com/rust-lang/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description "This package provides Rust compiler symbol demangling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atomic-polyfill-1
                       rust-cfg-if-1
                       rust-const-random-0.1
                       rust-getrandom-0.2
                       rust-once-cell-1
                       rust-serde-1
                       rust-version-check-0.9
                       rust-zerocopy-0.7)))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasmparser-0.215
  (package
    (name "rust-wasmparser")
    (version "0.215.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03nryws9f2awvfd47z5pn67aqif1z7w6z6zl3jw9jhgjh44dxysk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-ahash-0.8
                       rust-bitflags-2
                       rust-hashbrown-0.14
                       rust-indexmap-2
                       rust-semver-1
                       rust-serde-1)))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0  license:asl2.0
                   license:expat))))

(define-public rust-ruzstd-0.7
  (package
    (name "rust-ruzstd")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13xz8iv0c96m4mrcx9zmn1rimvfqprv641a3yabsf6wvc59v48jh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-byteorder-1
                       rust-twox-hash-1)))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format.")
    (license license:expat)))

(define-public rust-object-0.36
  (package
    (name "rust-object")
    (version "0.36.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nggchwvjgilrxarwcmfcisa9fq1phg382y672aa6lb86ir4kdi7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-compiler-builtins-0.1
                       rust-crc32fast-1
                       rust-flate2-1
                       rust-hashbrown-0.14
                       rust-indexmap-2
                       rust-memchr-2
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-ruzstd-0.7
                       rust-wasmparser-0.215)))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis "unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-jobserver-0.1
  (package
    (name "rust-jobserver")
    (version "0.1.32")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jobserver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-libc-0.2)))
    (home-page "https://github.com/rust-lang/jobserver-rs")
    (synopsis "An implementation of the GNU Make jobserver for Rust.")
    (description
     "This package provides An implementation of the GNU Make jobserver for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cc-1
  (package
    (name "rust-cc")
    (version "1.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14jjp993s17yfvl5linq3gs4jzb7f42nkq09r5kyfjskmjzsms79"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-jobserver-0.1
                       rust-libc-0.2)))
    (home-page "https://github.com/rust-lang/cc-rs")
    (synopsis
     "build-time dependency for Cargo build scripts to assist in invoking the native
C compiler to compile native C code into a static archive to be linked into Rust
code.")
    (description
     "This package provides a build-time dependency for Cargo build scripts to assist
in invoking the native C compiler to compile native C code into a static archive
to be linked into Rust code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasmparser-0.202
  (package
    (name "rust-wasmparser")
    (version "0.202.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasmparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qljgwjv6a6nn9sx6bbz167s0dim4liphgp1sc8ngygscaqb6fn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-bitflags-2
                       rust-indexmap-2
                       rust-semver-1)))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "simple event-driven library for parsing WebAssembly binary files.")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0 ))))

(define-public rust-ruzstd-0.6
  (package
    (name "rust-ruzstd")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ruzstd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yygqpar2x910lnii4k5p43aj4943hlnxpczmqhsfddmxrqa8x2i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-byteorder-1
                       rust-derive-more-0.99
                       rust-twox-hash-1)))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format.")
    (license license:expat)))

(define-public rust-object-0.35
  (package
    (name "rust-object")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pnv84mx3f3p847hfnsp4znivnwkc1x53maq459a92w42fw7mv5q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-compiler-builtins-0.1
                       rust-crc32fast-1
                       rust-flate2-1
                       rust-hashbrown-0.14
                       rust-indexmap-2
                       rust-memchr-2
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-ruzstd-0.6
                       rust-wasmparser-0.202)))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis "unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-memmap2-0.9
  (package
    (name "rust-memmap2")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmap2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08hkmvri44j6h14lyq4yw5ipsp91a9jacgiww4bs9jm8whi18xgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-libc-0.2
                       rust-stable-deref-trait-1)))
    (home-page "https://github.com/RazrFalcon/memmap2-rs")
    (synopsis "Cross-platform Rust API for memory-mapped file IO")
    (description
     "This package provides Cross-platform Rust API for memory-mapped file IO.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gimli-0.29
  (package
    (name "rust-gimli")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gimli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zgzprnjaawmg6zyic4f2q2hc39kdhn116qnkqpgvsasgc3x9v20"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-compiler-builtins-0.1
                       rust-fallible-iterator-0.3
                       rust-indexmap-2
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-stable-deref-trait-1)))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis "library for reading and writing the DWARF debugging format.")
    (description
     "This package provides a library for reading and writing the DWARF debugging
format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-addr2line-0.22
  (package
    (name "rust-addr2line")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "addr2line" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y66f1sa27i9kvmlh76ynk60rxfrmkba9ja8x527h32wdb206ibf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-compiler-builtins-0.1
                       rust-cpp-demangle-0.4
                       rust-fallible-iterator-0.3
                       rust-gimli-0.29
                       rust-memmap2-0.9
                       rust-object-0.35
                       rust-rustc-demangle-0.1
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-smallvec-1)))
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
     "cross-platform symbolication library written in Rust, using `gimli`")
    (description
     "This package provides a cross-platform symbolication library written in Rust,
using `gimli`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-backtrace-0.3
  (package
    (name "rust-backtrace")
    (version "0.3.73")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "backtrace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02iffg2pkg5nc36pgml8il7f77s138hhjw9f9l56v5zqlilk5hjw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-addr2line-0.22
                       rust-cc-1
                       rust-cfg-if-1
                       rust-cpp-demangle-0.4
                       rust-libc-0.2
                       rust-miniz-oxide-0.7
                       rust-object-0.36
                       rust-rustc-demangle-0.1
                       rust-serde-1
                       rust-winapi-0.3)))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
     "library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
     "This package provides a library to acquire a stack trace (backtrace) at runtime
in a Rust program.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anstyle-1
  (package
    (name "rust-anstyle")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "anstyle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cfmkza63xpn1kkz844mgjwm9miaiz4jkyczmwxzivcsypk1vv0v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-cli/anstyle")
    (synopsis "ANSI text styling")
    (description "This package provides ANSI text styling.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-builder-4
  (package
    (name "rust-clap-builder")
    (version "4.5.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dmas5z20yqmlmfhykr38pn1hkcnr4jzxjw4cs2f6lkn2wmyqsi1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-anstream-0.6
                       rust-anstyle-1
                       rust-backtrace-0.3
                       rust-clap-lex-0.7
                       rust-strsim-0.11
                       rust-terminal-size-0.3
                       rust-unicase-2
                       rust-unicode-width-0.1)))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.5.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k656mr99srcla2bx2h7wjwlb9mywavb5k1szpd5b9pxaj287n0i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-clap-builder-4
                       rust-clap-derive-4)))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-4
  (package
    (name "rust-clap-complete")
    (version "4.5.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_complete" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fhijpm4wdyh56xnvijvq3jzs4nqwpm2kylkn3c2cc3g2b87qrww"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-clap-4
                       rust-clap-lex-0.7
                       rust-is-executable-1
                       rust-pathdiff-0.2
                       rust-shlex-1
                       rust-unicode-xid-0.2)))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description
     "This package provides Generate shell completion scripts for your clap::Command.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-server-postgres-18
  (package
    (name "rust-atuin-server-postgres")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server-postgres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pc1h3qmmlm73jwyql3rdlbnxyq0xr0hcqvclkqy8xirrzqf43c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-trait-0.1
                       rust-atuin-common-18
                       rust-atuin-server-database-18
                       rust-eyre-0.6
                       rust-futures-util-0.3
                       rust-serde-1
                       rust-sqlx-0.7
                       rust-time-0.3
                       rust-tracing-0.1
                       rust-url-2
                       rust-uuid-1)))
    (home-page "https://atuin.sh")
    (synopsis "server postgres database library for atuin")
    (description
     "This package provides server postgres database library for atuin.")
    (license license:expat)))

(define-public rust-rustls-pemfile-2
  (package
    (name "rust-rustls-pemfile")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09bl873pkibmb2da49kkbm9jlagscjvzrv257q6k01p101my2vqr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-base64-0.22
                       rust-rustls-pki-types-1)))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
     "This package provides Basic .pem file parser for keys and certificates.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-sketches-ddsketch-0.2
  (package
    (name "rust-sketches-ddsketch")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sketches-ddsketch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p6n1v0p0773d0b5qnsnw526g7hhlb08bx95wm0zb09xnwa6qqw5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-serde-1)))
    (home-page "https://github.com/mheffner/rust-sketches-ddsketch")
    (synopsis "direct port of the Golang DDSketch implementation.")
    (description
     "This package provides a direct port of the Golang DDSketch implementation.")
    (license license:asl2.0)))

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
       #:cargo-inputs (rust-prost-0.11)))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "Prost definitions of Protocol Buffers well known types")
    (description
     "This package provides Prost definitions of Protocol Buffers well known types.")
    (license license:asl2.0)))

(define-public rust-quanta-0.11
  (package
    (name "rust-quanta")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quanta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-crossbeam-utils-0.8
                       rust-libc-0.2
                       rust-mach2-0.4
                       rust-once-cell-1
                       rust-prost-types-0.11
                       rust-raw-cpuid-10
                       rust-wasi-0.11
                       rust-web-sys-0.3
                       rust-winapi-0.3)))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description "This package provides high-speed timing library.")
    (license license:expat)))

(define-public rust-hashbrown-0.13
  (package
    (name "rust-hashbrown")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-ahash-0.8
                       rust-bumpalo-3
                       rust-compiler-builtins-0.1
                       rust-rayon-1
                       rust-rustc-std-workspace-alloc-1
                       rust-rustc-std-workspace-core-1
                       rust-serde-1)))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map.")
    (license (list license:expat license:asl2.0))))

(define-public rust-metrics-util-0.15
  (package
    (name "rust-metrics-util")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0glpkmrj7zkg9b290x6qxf93kmd9b4b4sbkk1fs19l8y95pfvqjd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-ahash-0.8
                       rust-aho-corasick-1
                       rust-crossbeam-epoch-0.9
                       rust-crossbeam-utils-0.8
                       rust-hashbrown-0.13
                       rust-indexmap-1
                       rust-metrics-0.21
                       rust-num-cpus-1
                       rust-ordered-float-3
                       rust-quanta-0.11
                       rust-radix-trie-0.2
                       rust-sketches-ddsketch-0.2)))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Helper types/functions used by the metrics ecosystem")
    (description
     "This package provides Helper types/functions used by the metrics ecosystem.")
    (license license:expat)))

(define-public rust-metrics-exporter-prometheus-0.12
  (package
    (name "rust-metrics-exporter-prometheus")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-exporter-prometheus" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l19s21jfmwm72cxfjq35xb79a5wi4fv7c1p993dnqj8gk7afkqx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-base64-0.21
                       rust-hyper-0.14
                       rust-indexmap-1
                       rust-ipnet-2
                       rust-metrics-0.21
                       rust-metrics-util-0.15
                       rust-quanta-0.11
                       rust-thiserror-1
                       rust-tokio-1
                       rust-tracing-0.1)))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "metrics-compatible exporter for sending metrics to Prometheus.")
    (description
     "This package provides a metrics-compatible exporter for sending metrics to
Prometheus.")
    (license license:expat)))

(define-public rust-metrics-macros-0.7
  (package
    (name "rust-metrics-macros")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0krmj7zyr4g14jdpk1jasi1w2nw64hqdxb2lfx4zxphp0vqgmd1q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-proc-macro2-1
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Macros for the metrics crate")
    (description "This package provides Macros for the metrics crate.")
    (license license:expat)))

(define-public rust-metrics-0.21
  (package
    (name "rust-metrics")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ibndxzk0sja8cgwrr73b9vzbgfvwzwxwkxqiivnmmwy00dazqzx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-ahash-0.8
                       rust-metrics-macros-0.7
                       rust-portable-atomic-1)))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "lightweight metrics facade.")
    (description "This package provides a lightweight metrics facade.")
    (license license:expat)))

(define-public rust-axum-server-0.6
  (package
    (name "rust-axum-server")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "axum-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dn0cx9ww1ph1dvljayhr62f898wl8xifpl3nsjg84jfxk1ldbf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-arc-swap-1
                       rust-bytes-1
                       rust-futures-util-0.3
                       rust-http-1
                       rust-http-body-1
                       rust-http-body-util-0.1
                       rust-hyper-1
                       rust-hyper-util-0.1
                       rust-openssl-0.10
                       rust-pin-project-lite-0.2
                       rust-rustls-0.21
                       rust-rustls-pemfile-2
                       rust-tokio-1
                       rust-tokio-openssl-0.6
                       rust-tokio-rustls-0.24
                       rust-tower-0.4
                       rust-tower-service-0.3)))
    (home-page "https://github.com/programatik29/axum-server")
    (synopsis "High level server designed to be used with axum framework")
    (description
     "This package provides High level server designed to be used with axum framework.")
    (license license:expat)))

(define-public rust-atuin-server-database-18
  (package
    (name "rust-atuin-server-database")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server-database" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jn6vk5rabc3z91khigcqkj0sry4ih2lzayvai9zdfi2pzrmrz6z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-trait-0.1
                       rust-atuin-common-18
                       rust-eyre-0.6
                       rust-serde-1
                       rust-time-0.3
                       rust-tracing-0.1
                       rust-uuid-1)))
    (home-page "https://atuin.sh")
    (synopsis "server database library for atuin")
    (description "This package provides server database library for atuin.")
    (license license:expat)))

(define-public rust-atuin-server-18
  (package
    (name "rust-atuin-server")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hic1c2bjpa7gldqrh055wcrcpkrl1nxc9jkf6w65ira458i6vx9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-argon2-0.5
                       rust-async-trait-0.1
                       rust-atuin-common-18
                       rust-atuin-server-database-18
                       rust-axum-0.7
                       rust-axum-server-0.6
                       rust-base64-0.22
                       rust-config-0.13
                       rust-eyre-0.6
                       rust-fs-err-2
                       rust-metrics-0.21
                       rust-metrics-exporter-prometheus-0.12
                       rust-rand-0.8
                       rust-reqwest-0.11
                       rust-rustls-0.21
                       rust-rustls-pemfile-2
                       rust-semver-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-time-0.3
                       rust-tokio-1
                       rust-tower-0.4
                       rust-tower-http-0.5
                       rust-tracing-0.1
                       rust-uuid-1)))
    (home-page "https://atuin.sh")
    (synopsis "server library for atuin")
    (description "This package provides server library for atuin.")
    (license license:expat)))

(define-public rust-tonic-types-0.11
  (package
    (name "rust-tonic-types")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x3420h5bg3k03m4r79h1dgnk90kjxrlgw5fqc7cdm6qf6a0iapl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-prost-0.12
                       rust-prost-types-0.12
                       rust-tonic-0.11)))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis
     "collection of useful protobuf types that can be used with `tonic`.")
    (description
     "This package provides a collection of useful protobuf types that can be used
with `tonic`.")
    (license license:expat)))

(define-public rust-tonic-build-0.11
  (package
    (name "rust-tonic-build")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic-build" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hm99ckaw0pzq8h22bdjy6gpbg06kpvs0f73nj60f456f3fzckmy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-prettyplease-0.2
                       rust-proc-macro2-1
                       rust-prost-build-0.12
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis "Codegen module of `tonic` gRPC implementation.")
    (description
     "This package provides Codegen module of `tonic` @code{gRPC} implementation.")
    (license license:expat)))

(define-public rust-tonic-0.11
  (package
    (name "rust-tonic")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qsr527i256i3dk9dp1g2jr42q7yl91y5h06rvd9ycy9rxfpi3n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-stream-0.3
                       rust-async-trait-0.1
                       rust-axum-0.6
                       rust-base64-0.21
                       rust-bytes-1
                       rust-flate2-1
                       rust-h2-0.3
                       rust-http-0.2
                       rust-http-body-0.4
                       rust-hyper-0.14
                       rust-hyper-timeout-0.4
                       rust-percent-encoding-2
                       rust-pin-project-1
                       rust-prost-0.12
                       rust-rustls-native-certs-0.7
                       rust-rustls-pemfile-2
                       rust-rustls-pki-types-1
                       rust-tokio-1
                       rust-tokio-rustls-0.25
                       rust-tokio-stream-0.1
                       rust-tower-0.4
                       rust-tower-layer-0.3
                       rust-tower-service-0.3
                       rust-tracing-0.1
                       rust-webpki-roots-0.26
                       rust-zstd-0.12)))
    (home-page "https://github.com/hyperium/tonic")
    (synopsis
     "gRPC over HTTP/2 implementation focused on high performance, interoperability, and flexibility.")
    (description
     "This package provides a @code{gRPC} over HTTP/2 implementation focused on high
performance, interoperability, and flexibility.")
    (license license:expat)))

(define-public rust-atuin-history-0.2
  (package
    (name "rust-atuin-history")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-history" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xsjhibw203pm8w15b2m1ck8mzbzsp6947pq2hyd63x5f6i70wdf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-trait-0.1
                       rust-atuin-client-18
                       rust-atuin-common-18
                       rust-base64-0.22
                       rust-crossterm-0.27
                       rust-directories-5
                       rust-eyre-0.6
                       rust-fs-err-2
                       rust-futures-util-0.3
                       rust-indicatif-0.17
                       rust-interim-0.1
                       rust-itertools-0.12
                       rust-log-0.4
                       rust-semver-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-sysinfo-0.30
                       rust-time-0.3
                       rust-tokio-1
                       rust-tracing-0.1
                       rust-unicode-segmentation-1
                       rust-unicode-width-0.1
                       rust-uuid-1
                       rust-whoami-1)))
    (home-page "https://atuin.sh")
    (synopsis "The history crate for Atuin")
    (description "This package provides The history crate for Atuin.")
    (license license:expat)))

(define-public rust-atuin-dotfiles-0.3
  (package
    (name "rust-atuin-dotfiles")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-dotfiles" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "086jxabi9brzcrpivz6v06ipgn21z5hrkp8926dzz2h29408l6px"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atuin-client-18
                       rust-atuin-common-18
                       rust-crypto-secretbox-0.1
                       rust-eyre-0.6
                       rust-rand-0.8
                       rust-rmp-0.8
                       rust-serde-1
                       rust-tokio-1)))
    (home-page "https://atuin.sh")
    (synopsis "The dotfiles crate for Atuin")
    (description "This package provides The dotfiles crate for Atuin.")
    (license license:expat)))

(define-public rust-atuin-daemon-0.2
  (package
    (name "rust-atuin-daemon")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-daemon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "039whjlg45m5dvcq8bbdjpwa501z5n1zjg76w74skha82i355711"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atuin-client-18
                       rust-atuin-dotfiles-0.3
                       rust-atuin-history-0.2
                       rust-dashmap-5
                       rust-eyre-0.6
                       rust-listenfd-1
                       rust-prost-0.12
                       rust-prost-types-0.12
                       rust-rand-0.8
                       rust-time-0.3
                       rust-tokio-1
                       rust-tokio-stream-0.1
                       rust-tonic-0.11
                       rust-tonic-build-0.11
                       rust-tonic-types-0.11
                       rust-tower-0.4
                       rust-tracing-0.1
                       rust-tracing-subscriber-0.3
                       rust-uuid-1)))
    (home-page "https://atuin.sh")
    (synopsis "The daemon crate for Atuin")
    (description "This package provides The daemon crate for Atuin.")
    (license license:expat)))

(define-public rust-wasite-0.1
  (package
    (name "rust-wasite")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ardaku/wasite/blob/stable/CHANGELOG.md")
    (synopsis "WASI Terminal Environment API")
    (description "This package provides WASI Terminal Environment API.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-whoami-1
  (package
    (name "rust-whoami")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "whoami" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aafr70h2zlqr73i58bj84hdf9rgplxbpygqbgsqhkk3mngv8jm4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-redox-syscall-0.4
                       rust-wasite-0.1
                       rust-web-sys-0.3)))
    (home-page "https://github.com/ardaku/whoami/blob/v1/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment")
    (description
     "This package provides Retrieve the current user and environment.")
    (license (list license:asl2.0 license:boost1.0 license:expat))))

(define-public rust-tiny-bip39-1
  (package
    (name "rust-tiny-bip39")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny-bip39" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-anyhow-1
                       rust-hmac-0.12
                       rust-once-cell-1
                       rust-pbkdf2-0.11
                       rust-rand-0.8
                       rust-rustc-hash-1
                       rust-sha2-0.10
                       rust-thiserror-1
                       rust-unicode-normalization-0.1
                       rust-wasm-bindgen-0.2
                       rust-zeroize-1)))
    (home-page "https://github.com/maciejhirsz/tiny-bip39/")
    (synopsis
     "fork of the bip39 crate with fixes to v0.6. Rust implementation of BIP-0039")
    (description
     "This package provides a fork of the bip39 crate with fixes to v0.6.  Rust
implementation of BIP-0039.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sql-builder-3
  (package
    (name "rust-sql-builder")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sql-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-anyhow-1
                       rust-thiserror-1)))
    (home-page "https://github.com/perdumonocle/sql-builder.git")
    (synopsis "Simple SQL code generator")
    (description "This package provides Simple SQL code generator.")
    (license license:expat)))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.124")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13g3zka165qqnvbw0c6k708wdfc4738sr4f8ijjxpcsnga265bb6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-indexmap-2
                       rust-itoa-1
                       rust-memchr-2
                       rust-ryu-1
                       rust-serde-1)))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-typeid-1
  (package
    (name "rust-typeid")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typeid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ky97g0dwzdhmbcwzy098biqh26vhlc98l5x6zy44yhyk7687785"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/typeid")
    (synopsis "Const TypeId and non-'static TypeId")
    (description
     "This package provides Const @code{TypeId} and non-'static @code{TypeId}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-erased-serde-0.4
  (package
    (name "rust-erased-serde")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "erased-serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13dirfj9972nvk05b20w3xyn3xp1j6qyfp9avhksnkxbcnfkiqi4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-serde-1
                       rust-typeid-1)))
    (home-page "https://github.com/dtolnay/erased-serde")
    (synopsis "Type-erased Serialize and Serializer traits")
    (description
     "This package provides Type-erased Serialize and Serializer traits.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rusty-paseto-0.7
  (package
    (name "rust-rusty-paseto")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusty_paseto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cvcc15cs2slv76r4k5fysjvr1ag1kbcnzgvd3d47ppjipsbja0c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-aes-0.7
                       rust-base64-0.22
                       rust-blake2-0.10
                       rust-chacha20-0.9
                       rust-chacha20poly1305-0.10
                       rust-digest-0.10
                       rust-ed25519-dalek-2
                       rust-erased-serde-0.4
                       rust-hex-0.4
                       rust-hmac-0.12
                       rust-iso8601-0.6
                       rust-p384-0.13
                       rust-rand-core-0.6
                       rust-ring-0.17
                       rust-serde-1
                       rust-serde-json-1
                       rust-sha2-0.10
                       rust-thiserror-1
                       rust-time-0.3
                       rust-zeroize-1)))
    (home-page "https://github.com/rrrodzilla/rusty_paseto")
    (synopsis
     "type-driven, ergonomic alternative to JWT for secure stateless PASETO tokens.")
    (description
     "This package provides a type-driven, ergonomic alternative to JWT for secure
stateless PASETO tokens.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rusty-paserk-0.4
  (package
    (name "rust-rusty-paserk")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusty_paserk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f0xqrjbvx7mb2ynnqni9ql8qlg3zzn504vnyjmyh7ilrlgailx1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-aes-0.8
                       rust-arbitrary-1
                       rust-argon2-0.5
                       rust-base64-0.22
                       rust-base64ct-1
                       rust-blake2-0.10
                       rust-chacha20-0.9
                       rust-cipher-0.4
                       rust-ctr-0.9
                       rust-curve25519-dalek-4
                       rust-digest-0.10
                       rust-ed25519-dalek-2
                       rust-generic-array-0.14
                       rust-hmac-0.12
                       rust-p384-0.13
                       rust-pbkdf2-0.12
                       rust-rand-0.8
                       rust-rusty-paseto-0.7
                       rust-serde-1
                       rust-sha2-0.10
                       rust-subtle-2)))
    (home-page "https://github.com/conradludgate/rusty-paserk")
    (synopsis
     "Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key management")
    (description
     "This package provides Platform Agnostic Serializable Keys (PASERK) is an extension on PASETO for key
management.")
    (license license:expat)))

(define-public rust-rmp-0.8
  (package
    (name "rust-rmp")
    (version "0.8.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rmp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-byteorder-1
                       rust-num-traits-0.2
                       rust-paste-1)))
    (home-page "https://github.com/3Hren/msgpack-rust")
    (synopsis "Pure Rust MessagePack serialization implementation")
    (description
     "This package provides Pure Rust @code{MessagePack} serialization implementation.")
    (license license:expat)))

(define-public rust-minspan-0.1
  (package
    (name "rust-minspan")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "minspan" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0053r44iqmfilibz8da3367adxjjwibw6d849xifxq0yhfgf99pf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/mwotton/minspan")
    (synopsis
     "a package for determining the minimum span of one vector within another")
    (description
     "This package provides a package for determining the minimum span of one vector within another.")
    (license license:expat)))

(define-public rust-logos-codegen-0.14
  (package
    (name "rust-logos-codegen")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nliw1ka842hf5kl4d3j7gcs611v2g8z5ylw5v6ba36lgg5ndglh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-beef-0.5
                       rust-fnv-1
                       rust-lazy-static-1
                       rust-proc-macro2-1
                       rust-quote-1
                       rust-regex-syntax-0.8
                       rust-syn-2)))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-derive-0.14
  (package
    (name "rust-logos-derive")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gccx82s4fm10fk360zqxjszr3rgy5c9w0lljjrqcrg9x0ql45a5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-logos-codegen-0.14)))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-logos-0.14
  (package
    (name "rust-logos")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "logos" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qyn0gldda1hxqp31wl0l4sacnnsrqnmbgiv36yxgdr1qcyn77z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-logos-derive-0.14)))
    (home-page "https://logos.maciej.codes/")
    (synopsis "Create ridiculously fast Lexers")
    (description "This package provides Create ridiculously fast Lexers.")
    (license (list license:expat license:asl2.0))))

(define-public rust-interim-0.1
  (package
    (name "rust-interim")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "interim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x5ykyv8bkv13398q3dpycg5943rw1jycvjbhi2yih30zw5hzzcs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-chrono-0.4
                       rust-logos-0.14
                       rust-time-0.3)))
    (home-page "https://github.com/conradludgate/interim")
    (synopsis
     "parses simple English dates, inspired by Linux date command, and forked from chrono-english")
    (description
     "This package provides parses simple English dates, inspired by Linux date command, and forked from
chrono-english.")
    (license license:expat)))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid-macro-internal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0flsk6rxdif2hx5dirfwfn6r5vrp6my9zvnn43lw98iyz13d077f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-proc-macro2-1
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "Private implementation details of the uuid! macro")
    (description
     "This package provides Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uuid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0503gvp08dh5mnm3f0ffqgisj6x3mbs53dmnn1lm19pga43a1pw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-arbitrary-1
                       rust-atomic-0.6
                       rust-borsh-1
                       rust-borsh-derive-1
                       rust-bytemuck-1
                       rust-getrandom-0.2
                       rust-md-5-0.10
                       rust-rand-0.8
                       rust-serde-1
                       rust-sha1-smol-1
                       rust-slog-2
                       rust-uuid-macro-internal-1
                       rust-wasm-bindgen-0.2
                       rust-zerocopy-0.7)))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-typed-builder-macro-0.18
  (package
    (name "rust-typed-builder-macro")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-builder-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qwfq0q2lkg4bkmcpsqajy3ss2sb2h47dj5zhfwvbp27ygx8sw8z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-proc-macro2-1
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://github.com/idanarye/rust-typed-builder")
    (synopsis "Compile-time type-checked builder derive")
    (description
     "This package provides Compile-time type-checked builder derive.")
    (license (list license:expat license:asl2.0))))

(define-public rust-typed-builder-0.18
  (package
    (name "rust-typed-builder")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p9s9p7f3mnylrzdqbxj73d9dw95syma6pnnyfp3ys801s49qwvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-typed-builder-macro-0.18)))
    (home-page "https://github.com/idanarye/rust-typed-builder")
    (synopsis "Compile-time type-checked builder derive")
    (description
     "This package provides Compile-time type-checked builder derive.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kqwxvfh2jkpg38fy673d6danh1bhcmmbsmffww3mphgail2l99z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-num-conv-0.1
                       rust-time-core-0.1)))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.")
    (description
     "This package provides Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.36")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11g8hdpahgrf1wwl2rpsg5nxq3aj7ri6xr672v4qcij6cgjqizax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-deranged-0.3
                       rust-itoa-1
                       rust-js-sys-0.3
                       rust-libc-0.2
                       rust-num-conv-0.1
                       rust-num-threads-0.1
                       rust-powerfmt-0.2
                       rust-quickcheck-1
                       rust-rand-0.8
                       rust-serde-1
                       rust-time-core-0.1
                       rust-time-macros-0.2)))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std]")
    (description
     "This package provides Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-flume-0.11
  (package
    (name "rust-flume")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flume" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10girdbqn77wi802pdh55lwbmymy437k7kklnvj12aaiwaflbb2m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-futures-core-0.3
                       rust-futures-sink-0.3
                       rust-nanorand-0.7
                       rust-spin-0.9)))
    (home-page "https://github.com/zesterer/flume")
    (synopsis "blazingly fast multi-producer channel")
    (description
     "This package provides a blazingly fast multi-producer channel.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlx-sqlite-0.7
  (package
    (name "rust-sqlx-sqlite")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ap0bb2hazbrdgd7mhnckdg9xcchx0k094di9gnhpnhlhh5fyi5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atoi-2
                       rust-chrono-0.4
                       rust-flume-0.11
                       rust-futures-channel-0.3
                       rust-futures-core-0.3
                       rust-futures-executor-0.3
                       rust-futures-intrusive-0.5
                       rust-futures-util-0.3
                       rust-libsqlite3-sys-0.27
                       rust-log-0.4
                       rust-percent-encoding-2
                       rust-regex-1
                       rust-serde-1
                       rust-sqlx-core-0.7
                       rust-time-0.3
                       rust-tracing-0.1
                       rust-url-2
                       rust-urlencoding-2
                       rust-uuid-1)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "SQLite driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details")
    (description
     "This package provides SQLite driver implementation for SQLx.  Not for direct use; see the `sqlx` crate
for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-postgres-0.7
  (package
    (name "rust-sqlx-postgres")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-postgres" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zjp30wj4n2f25dnb32vsg6jfpa3gw6dmfd0i5pr4kw91fw4x0kw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atoi-2
                       rust-base64-0.21
                       rust-bigdecimal-0.3
                       rust-bit-vec-0.6
                       rust-bitflags-2
                       rust-byteorder-1
                       rust-chrono-0.4
                       rust-crc-3
                       rust-dotenvy-0.15
                       rust-etcetera-0.8
                       rust-futures-channel-0.3
                       rust-futures-core-0.3
                       rust-futures-io-0.3
                       rust-futures-util-0.3
                       rust-hex-0.4
                       rust-hkdf-0.12
                       rust-hmac-0.12
                       rust-home-0.5
                       rust-ipnetwork-0.20
                       rust-itoa-1
                       rust-log-0.4
                       rust-mac-address-1
                       rust-md-5-0.10
                       rust-memchr-2
                       rust-num-bigint-0.4
                       rust-once-cell-1
                       rust-rand-0.8
                       rust-rust-decimal-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-sha2-0.10
                       rust-smallvec-1
                       rust-sqlx-core-0.7
                       rust-stringprep-0.1
                       rust-thiserror-1
                       rust-time-0.3
                       rust-tracing-0.1
                       rust-uuid-1
                       rust-whoami-1)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "PostgreSQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details")
    (description
     "This package provides @code{PostgreSQL} driver implementation for SQLx.  Not for direct use; see the
`sqlx` crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-mysql-0.7
  (package
    (name "rust-sqlx-mysql")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-mysql" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "066lxhb80xgb8r5m2yy3a7ydjvp0b6wsk9s7whwfa83d46817lqy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-atoi-2
                       rust-base64-0.21
                       rust-bigdecimal-0.3
                       rust-bitflags-2
                       rust-byteorder-1
                       rust-bytes-1
                       rust-chrono-0.4
                       rust-crc-3
                       rust-digest-0.10
                       rust-dotenvy-0.15
                       rust-either-1
                       rust-futures-channel-0.3
                       rust-futures-core-0.3
                       rust-futures-io-0.3
                       rust-futures-util-0.3
                       rust-generic-array-0.14
                       rust-hex-0.4
                       rust-hkdf-0.12
                       rust-hmac-0.12
                       rust-itoa-1
                       rust-log-0.4
                       rust-md-5-0.10
                       rust-memchr-2
                       rust-once-cell-1
                       rust-percent-encoding-2
                       rust-rand-0.8
                       rust-rsa-0.9
                       rust-rust-decimal-1
                       rust-serde-1
                       rust-sha1-0.10
                       rust-sha2-0.10
                       rust-smallvec-1
                       rust-sqlx-core-0.7
                       rust-stringprep-0.1
                       rust-thiserror-1
                       rust-time-0.3
                       rust-tracing-0.1
                       rust-uuid-1
                       rust-whoami-1)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "MySQL driver implementation for SQLx. Not for direct use; see the `sqlx` crate for details")
    (description
     "This package provides @code{MySQL} driver implementation for SQLx.  Not for direct use; see the `sqlx`
crate for details.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-core-0.7
  (package
    (name "rust-sqlx-macros-core")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-macros-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j7k0fw7n6pgabqnj6cbp8s3rmd3yvqr4chjj878cvd1m99yycsq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-std-1
                       rust-dotenvy-0.15
                       rust-either-1
                       rust-heck-0.4
                       rust-hex-0.4
                       rust-once-cell-1
                       rust-proc-macro2-1
                       rust-quote-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-sha2-0.10
                       rust-sqlx-core-0.7
                       rust-sqlx-mysql-0.7
                       rust-sqlx-postgres-0.7
                       rust-sqlx-sqlite-0.7
                       rust-syn-1
                       rust-tempfile-3
                       rust-tokio-1
                       rust-url-2)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Macro support core for SQLx, the Rust SQL toolkit. Not intended to be used directly")
    (description
     "This package provides Macro support core for SQLx, the Rust SQL toolkit.  Not intended to be used
directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-macros-0.7
  (package
    (name "rust-sqlx-macros")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09rih250868nfkax022y5dyk24a7qfw6scjy3sgalbzb8lihx92f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-proc-macro2-1
                       rust-quote-1
                       rust-sqlx-core-0.7
                       rust-sqlx-macros-core-0.7
                       rust-syn-1)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Macros for SQLx, the rust SQL toolkit. Not intended to be used directly")
    (description
     "This package provides Macros for SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlformat-0.2
  (package
    (name "rust-sqlformat")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlformat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07vwxjfcbdnbzsg3683mshjc245rr4k8j9b6zvqmbk0q8dry75gq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-nom-7
                       rust-unicode-categories-0.1)))
    (home-page "https://github.com/shssoichiro/sqlformat-rs")
    (synopsis "Formats whitespace in a SQL string to make it easier to read")
    (description
     "This package provides Formats whitespace in a SQL string to make it easier to read.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-intrusive-0.5
  (package
    (name "rust-futures-intrusive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-intrusive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vwm08d1pli6bdaj0i7xhk3476qlx4pll6i0w03gzdnh7lh0r4qx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-futures-core-0.3
                       rust-lock-api-0.4
                       rust-parking-lot-0.12)))
    (home-page "https://github.com/Matthias247/futures-intrusive")
    (synopsis
     "Futures based on intrusive data structures - for std and no-std environments.")
    (description
     "This package provides Futures based on intrusive data structures - for std and no-std environments.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-core-0.7
  (package
    (name "rust-sqlx-core")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xiyr35dq10sf7lq00291svcj9wbaaz1ihandjmrng9a6jlmkfi4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-ahash-0.8
                       rust-async-io-1
                       rust-async-std-1
                       rust-atoi-2
                       rust-bigdecimal-0.3
                       rust-bit-vec-0.6
                       rust-bstr-1
                       rust-byteorder-1
                       rust-bytes-1
                       rust-chrono-0.4
                       rust-crc-3
                       rust-crossbeam-queue-0.3
                       rust-digest-0.10
                       rust-either-1
                       rust-encoding-rs-0.8
                       rust-event-listener-2
                       rust-futures-channel-0.3
                       rust-futures-core-0.3
                       rust-futures-intrusive-0.5
                       rust-futures-io-0.3
                       rust-futures-util-0.3
                       rust-hashlink-0.8
                       rust-hex-0.4
                       rust-indexmap-2
                       rust-ipnetwork-0.20
                       rust-log-0.4
                       rust-mac-address-1
                       rust-memchr-2
                       rust-native-tls-0.2
                       rust-num-bigint-0.4
                       rust-once-cell-1
                       rust-paste-1
                       rust-percent-encoding-2
                       rust-regex-1
                       rust-rust-decimal-1
                       rust-rustls-0.21
                       rust-rustls-pemfile-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-sha1-0.10
                       rust-sha2-0.10
                       rust-smallvec-1
                       rust-sqlformat-0.2
                       rust-thiserror-1
                       rust-time-0.3
                       rust-tokio-1
                       rust-tokio-stream-0.1
                       rust-tracing-0.1
                       rust-url-2
                       rust-uuid-1
                       rust-webpki-roots-0.25)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "Core of SQLx, the rust SQL toolkit. Not intended to be used directly")
    (description
     "This package provides Core of SQLx, the rust SQL toolkit.  Not intended to be used directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlx-0.7
  (package
    (name "rust-sqlx")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ahadprvyhjraq0c5712x3kdkp1gkwfm9nikrmcml2h03bzwr8n9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-sqlx-core-0.7
                       rust-sqlx-macros-0.7
                       rust-sqlx-mysql-0.7
                       rust-sqlx-postgres-0.7
                       rust-sqlx-sqlite-0.7)))
    (home-page "https://github.com/launchbadge/sqlx")
    (synopsis
     "SQLX, The Rust SQL Toolkit. An async, pure Rust SQL crate featuring compile-time checked queries without a DSL. Supports PostgreSQL, MySQL, and SQLite")
    (description
     "This package provides SQLX The Rust SQL Toolkit.  An async, pure Rust SQL crate featuring compile-time
checked queries without a DSL. Supports @code{PostgreSQL}, @code{MySQL}, and
SQLite.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.207")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03m08jmb5zqdz3bynvz4n4kyqm6ymq5k836wrzr8w3kbr0s2dska"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-proc-macro2-1
                       rust-quote-1
                       rust-syn-2)))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "This package provides Macros 1.1 implementation of #[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.207")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wl4wx1lgw90rj63i551rzvy36gjm6yd79wv0a8ipsm4955f2ran"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-serde-derive-1
                       rust-serde-derive-1)))
    (home-page "https://serde.rs")
    (synopsis "generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atuin-common-18
  (package
    (name "rust-atuin-common")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-common" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0icymaqp7530lmqk0wjny6kgzzlylbix79w8mpycmz0165li5v66"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-eyre-0.6
                       rust-lazy-static-1
                       rust-rand-0.8
                       rust-semver-1
                       rust-serde-1
                       rust-sqlx-0.7
                       rust-sysinfo-0.30
                       rust-thiserror-1
                       rust-time-0.3
                       rust-typed-builder-0.18
                       rust-uuid-1)))
    (home-page "https://atuin.sh")
    (synopsis "common library for atuin")
    (description "This package provides common library for atuin.")
    (license license:expat)))

(define-public rust-atuin-client-18
  (package
    (name "rust-atuin-client")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vmqcqf629pfzxqiinr6pvwvjlgy8gyl7wap4g517wxczr70vbr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (rust-async-trait-0.1
                       rust-atuin-common-18
                       rust-base64-0.22
                       rust-clap-4
                       rust-config-0.13
                       rust-crypto-secretbox-0.1
                       rust-directories-5
                       rust-eyre-0.6
                       rust-fs-err-2
                       rust-futures-0.3
                       rust-generic-array-0.14
                       rust-hex-0.4
                       rust-humantime-2
                       rust-indicatif-0.17
                       rust-interim-0.1
                       rust-itertools-0.12
                       rust-log-0.4
                       rust-memchr-2
                       rust-minspan-0.1
                       rust-rand-0.8
                       rust-regex-1
                       rust-reqwest-0.11
                       rust-rmp-0.8
                       rust-rusty-paserk-0.4
                       rust-rusty-paseto-0.7
                       rust-semver-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-serde-regex-1
                       rust-serde-with-3
                       rust-sha2-0.10
                       rust-shellexpand-3
                       rust-sql-builder-3
                       rust-sqlx-0.7
                       rust-thiserror-1
                       rust-time-0.3
                       rust-tiny-bip39-1
                       rust-tokio-1
                       rust-typed-builder-0.18
                       rust-urlencoding-2
                       rust-uuid-1
                       rust-whoami-1)))
    (home-page "https://atuin.sh")
    (synopsis "client library for atuin")
    (description "This package provides client library for atuin.")
    (license license:expat)))

(define-public atuin
  (package
    (name "atuin")
    (version "18.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atuin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jyv71m50klh1n1h365rr9q2jl4agn42bcsfaqakn8nn9736ydzq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; wants to connect to "postgres://atuin:****@localhost:5432/atuin"
       #:cargo-inputs (rust-async-trait-0.1
                       rust-atuin-client-18
                       rust-atuin-common-18
                       rust-atuin-daemon-0.2
                       rust-atuin-dotfiles-0.3
                       rust-atuin-history-0.2
                       rust-atuin-server-18
                       rust-atuin-server-postgres-18
                       rust-base64-0.22
                       rust-clap-4
                       rust-clap-complete-4
                       rust-clap-complete-nushell-4
                       rust-cli-clipboard-0.4
                       rust-colored-2
                       rust-crossterm-0.27
                       rust-directories-5
                       rust-env-logger-0.11
                       rust-eyre-0.6
                       rust-fs-err-2
                       rust-futures-util-0.3
                       rust-fuzzy-matcher-0.3
                       rust-indicatif-0.17
                       rust-interim-0.1
                       rust-itertools-0.12
                       rust-log-0.4
                       rust-ratatui-0.26
                       rust-regex-1
                       rust-rpassword-7
                       rust-runtime-format-0.1
                       rust-rustix-0.38
                       rust-semver-1
                       rust-serde-1
                       rust-serde-json-1
                       rust-sysinfo-0.30
                       rust-time-0.3
                       rust-tiny-bip39-1
                       rust-tokio-1
                       rust-tracing-0.1
                       rust-tracing-subscriber-0.3
                       rust-unicode-segmentation-1
                       rust-unicode-width-0.1
                       rust-uuid-1
                       rust-whoami-1)
       #:cargo-development-inputs (rust-tracing-tree-0.3)))
    (native-inputs (list protobuf))
    (home-page "https://atuin.sh")
    (synopsis "atuin - magical shell history")
    (description "This package provides atuin - magical shell history.")
    (license license:expat)))

(define libstdc++
  ;; Libstdc++ matching the default GCC.
  (make-libstdc++ gcc))

(define-public atuin-bin
  (package
    (name "atuin-bin")
    (version "18.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/atuinsh/atuin/releases/download/v" version
             "/atuin-v" version "-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "1pk8jchf6kxa01qbv3amzwwkj0fxc35yv446igc1h6z870yanj58"))))
    (build-system binary-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'binary-unpack)
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (invoke "tar" "xvzf"
                              (assoc-ref inputs "source") "--strip-components"
                              "1"))))
       #:install-plan `(("atuin" "/bin/")
                        ("completions/atuin.fish"
                         "/share/fish/vendor_completions.d/")
                        ("completions/atuin.bash"
                         "/share/bash-completion/completions/")
                        ("completions/_atuin" "/share/zsh/site-functions/"))
       #:patchelf-plan `(("atuin" ("glibc" "libgccjit" "libstdc++")))))
    (inputs `(curl
              libgccjit
              libstdc++
              glibc
              zlib))
    (synopsis "Sync, search and backup shell history")
    (description
     "Atuin replaces your existing shell history with a SQLite database, and records additional context for your commands. Additionally, it provides optional and fully encrypted synchronisation of your history between machines, via an Atuin server.")
    (home-page "https://atuin.sh/")
    (license license:expat)))
