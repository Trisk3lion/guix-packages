(define-module (trisk packages weidu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages rust)
  #:use-module (trisk packages rust-crates))


(define-public mod-installer
  (package
    (name "mod-installer")
    (version "12.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dark0dave/mod_installer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n248ncxhh9x1iaggy9w0rgzmazzk2jwxx7jiv3ml9wb9z6xmfz5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:rust ,rust-1.88))
    (native-inputs (list pkg-config))
    (inputs (cons* openssl
                   `(,zstd "lib")
                   (cargo-inputs 'mod-installer #:module '(trisk packages rust-crates))))
    (home-page "https://github.com/dark0dave/mod_installer")
    (synopsis
     "The Infinity Engine Mod Installer is a tool designed to automate the installation of mods for Infinity Engine games such as Baldur's Gate, Icewind Dale, and Planescape: Torment. It uses a file called 'weidu.log' to determine which mods to install and how to install them")
    (description
     "This package provides The Infinity Engine Mod Installer is a tool designed to automate the
installation of mods for Infinity Engine games such as Baldur's Gate, Icewind
Dale, and Planescape: Torment.  It uses a file called weidu.log to determine
which mods to install and how to install them.")
    (properties
     `((upstream-name . "mod_installer")))
    (license license:expat)))

(define-public elkhound
  (package
    (name "elkhound")
    (version "0-unstable-2020-04-13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WeiDUorg/elkhound")
             (commit "a7eb4bb2151c00cc080613a770d37560f62a285c")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a03v8vngxxipdn06q36hi1kbqi6kn16f15amkpkvb2dj0b8xpk3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:configure-flags #~(list "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-build
            (lambda _
              ;; Save the source root directory for later reference
              (setenv "ELKHOUND_SOURCE_ROOT" (getcwd))
              ;; The CMake build system expects to be run from the src/ directory
              (chdir "src")))
          (add-after 'prepare-build 'patch-shebangs-and-cmake
            (lambda _
              ;; Patch script shebangs to use store paths
              (for-each
               (lambda (script)
                 (substitute* script
                   (("^#!/usr/bin/env perl")
                    (string-append "#!" (which "perl")))
                   (("^#!/bin/sh")
                    (string-append "#!" (which "sh")))))
               (find-files "scripts"))
              ;; Update CMake minimum version for compatibility with Guix
              (substitute* "CMakeLists.txt"
                (("cmake_minimum_required\\(VERSION 3\\.0\\)")
                 "cmake_minimum_required(VERSION 3.10)"))))
          (replace 'install
            (lambda _
              (let* ((out #$output)
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (doc (string-append out "/share/doc/elkhound"))
                     (src-root (getenv "ELKHOUND_SOURCE_ROOT"))
                     (src-dir (string-append src-root "/src")))
                ;; Install executables
                (install-file "ast/astgen" bin)
                (install-file "elkhound/elkhound" bin)
                ;; Install static libraries
                (for-each
                 (lambda (subdir)
                   (for-each
                    (lambda (archive)
                      (install-file archive lib))
                    (find-files subdir "\\.a$")))
                 '("ast" "elkhound" "smbase"))
                ;; Install header files
                (for-each
                 (lambda (subdir)
                   (let ((include-dir (string-append out "/include/" subdir))
                         (header-dir (string-append src-dir "/" subdir)))
                     (for-each
                      (lambda (header)
                        (install-file header include-dir))
                      (find-files header-dir "\\.h$"))))
                 '("ast" "elkhound" "smbase"))
                ;; Install documentation
                (for-each
                 (lambda (txt-file)
                   (install-file txt-file doc))
                 (find-files (string-append src-dir "/elkhound") "\\.txt$"))))))))
    (native-inputs
     (list bison flex perl))
    (home-page "https://scottmcpeak.com/elkhound/")
    (synopsis "GLR parser generator")
    (description
     "Elkhound is a parser generator that emits GLR (Generalized Left-to-Right
Rightmost derivation) parsers.  It can generate parsers in either OCaml or C++.

GLR parsers can handle ambiguous grammars by exploring all possible parses
simultaneously, making them more flexible than traditional LR parsers.  This
makes Elkhound particularly suitable for parsing complex or ambiguous languages.

The package provides two main programs:

@itemize
@item @command{elkhound}: The GLR parser generator
@item @command{astgen}: Abstract Syntax Tree generator utility
@end itemize

Additionally, static libraries and C++ header files are included for developing
parsers with Elkhound.  Elkhound is notably used by WeiDU for parsing Infinity
Engine file formats and scripting languages.")
    (license license:bsd-3)))

(define ocaml-unsafe-string
  (package
    (inherit ocaml-4.14)
    (name "ocaml-unsafe-string")
    (arguments
     (substitute-keyword-arguments (package-arguments ocaml-4.14)
       ((#:configure-flags flags #~'())
        #~(cons* "--disable-force-safe-string" "DEFAULT_STRING=unsafe" #$flags))))
    (synopsis "OCaml with unsafe (mutable) strings")
    (description
     "This is OCaml 4.14 compiled without forced safe strings.  Some legacy
programs, including WeiDU, require mutable strings and will not work with
the standard OCaml configuration that enforces immutable strings.

Note: This variant should only be used for software that absolutely requires
mutable strings and cannot be updated to use modern OCaml string handling.")))

(define-public weidu
  (package
    (name "weidu")
    (version "249.00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WeiDUorg/weidu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vp2kcpfvwqagidwz0l66a2z3w6h02g9kznjv7c0qx2r657hmygs"))
              (patches
               (list
                ;; Patch for OCaml 4.14 compatibility
                (origin
                  (method url-fetch)
                  (uri "https://github.com/WeiDUorg/weidu/commit/bb90190d8bf7d102952c07d8288a7dc6c7a3322e.patch")
                  (sha256
                   (base32 "0cfywl6cza0kvaf84zplvw4m2zj0lvciw7mpyn22slckshhi8mc1")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f  ; No test suite in the repository
      #:parallel-build? #f
      #:make-flags
      #~(list "weidu" "weinstall" "tolower")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)  ; No configure script

          (add-after 'unpack 'patch-and-config
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Create Configuration file from sample
              (copy-file "sample.Configuration" "Configuration")

              ;; Set correct paths in Configuration file
              (let ((ocaml-bin (string-append (assoc-ref inputs "ocaml-unsafe-string") "/bin"))
                    (elkhound (search-input-file inputs "/bin/elkhound")))
                (substitute* "Configuration"
                  (("/usr/bin") ocaml-bin)
                  (("/usr/local/bin") ocaml-bin)
                  ;; Point to actual elkhound binary
                  (("elkhound") elkhound)))

              ;; Create necessary build directories
              (mkdir-p "obj/.depend")
              (mkdir-p "obj/x86_LINUX")

              ;; Fix hashtbl compatibility issue with OCaml 4.14+
              ;; This prevents "undefined reference to `caml_hash_univ_param'" errors
              (let ((hashtbl-file "hashtbl-4.03.0/myhashtbl.ml"))
                (substitute* hashtbl-file
                  ;; Delete line 20, 21 and replace old_hash_param with hash_param
                  (("external old_hash_param :") "")
                  ((".*caml_hash_univ_param.*") "")
                  (("old_hash_param") "hash_param")))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (doc (string-append #$output "/share/doc/weidu")))
                ;; Install the three main binaries
                ;; Binaries are named *.asm.exe
                (for-each
                 (lambda (prog)
                   (let* ((exe-name (string-append prog ".asm.exe"))
                         (dest-name (string-append bin "/" exe-name))
                         (new-name (string-append bin "/" prog)))
                       (install-file exe-name bin)
                       ;; Rename from .asm.exe to just the program name
                       (rename-file dest-name new-name)
                       ;; Make executable
                       (unless (executable-file? new-name)
                         (chmod new-name #o555))))
                 '("weidu" "weinstall" "tolower"))
                ;; Install documentation
                (for-each
                 (lambda (file)
                   (install-file file doc))
                 '("README.md" "COPYING" "README-WeiDU-Changes.txt"))))))))
    (native-inputs (list elkhound
                         perl
                         which))

    (inputs (list ocaml-unsafe-string  ;  Weidu relias on unsafe strings!
                  zlib))

    (home-page "https://weidu.org/")
    (synopsis "InfinityEngine modding tool")
    (description
     "WeiDU (Weimer Dialogue Utility) is a program used to develop, distribute,
and install modifications (mods) for games based on the Infinity Engine.
Supported games include:

@itemize
@item Baldur's Gate and Baldur's Gate II (including Enhanced Editions)
@item Icewind Dale and Icewind Dale II (including Enhanced Editions)
@item Planescape: Torment (including Enhanced Edition)
@end itemize

WeiDU provides a high-level scripting language (WeiDU DSL) for describing game
modifications.  It handles binary patching of game files, manages complex mod
installation dependencies, tracks installation order, and provides automatic
backup and restoration capabilities.

The package includes three command-line programs:

@itemize
@item @command{weidu}: The main modding tool for installing and developing mods
@item @command{weinstall}: Uninstalls previously installed mods
@item @command{tolower}: Converts game filenames to lowercase, required when
running Windows-based Infinity Engine games on case-sensitive filesystems
@end itemize

WeiDU has become the de facto standard for Infinity Engine modding, with
thousands of community mods relying on it.")

    (properties
     `((upstream-name . "WeiDU")))

    (license license:gpl2)))

(define-public weidu-latest
  (let ((rev "6143572e42111ca0daa5feed30b70c214214e40a"))
    (package
      (name "weidu-latest")
      (version (git-version "249" "1" (string-drop-right rev 33)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/WeiDUorg/weidu")
                       (commit rev)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18dml206cf94f7bxgnyww0ba0dp50zcsjqs3yx7c0bv7bx93m0d5"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f  ; No test suite in the repository
        #:parallel-build? #f
        #:make-flags
        #~(list "weidu" "weinstall" "tolower")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)  ; No configure script

            (add-after 'unpack 'patch-and-config
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Set correct paths in Configuration file
                (let ((ocaml-bin (string-append (assoc-ref inputs "ocaml-unsafe-string") "/bin"))
                      (elkhound (search-input-file inputs "/bin/elkhound")))
                  (substitute* "Configuration"
                    (("/usr/bin") ocaml-bin)
                    (("/usr/local/bin") ocaml-bin)
                    ;; Point to actual elkhound binary
                    (("elkhound") elkhound)))

                ;; Create necessary build directories
                (mkdir-p "obj/.depend")
                (mkdir-p "obj/x86_LINUX")))
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin"))
                      (doc (string-append #$output "/share/doc/weidu")))
                  ;; Install the three main binaries
                  ;; Binaries are named *.asm.exe
                  (for-each
                   (lambda (prog)
                     (let* ((exe-name (string-append prog ".asm.exe"))
                            (dest-name (string-append bin "/" exe-name))
                            (new-name (string-append bin "/" prog)))
                       (install-file exe-name bin)
                       ;; Rename from .asm.exe to just the program name
                       (rename-file dest-name new-name)
                       ;; Make executable
                       (unless (executable-file? new-name)
                         (chmod new-name #o555))))
                   '("weidu" "weinstall" "tolower"))
                  ;; Install documentation
                  (for-each
                   (lambda (file)
                     (install-file file doc))
                   '("README.md" "COPYING" "README-WeiDU-Changes.txt"))))))))
      (native-inputs (list elkhound
                           perl
                           which))

      (inputs (list ocaml-unsafe-string  ;  Weidu relias on unsafe strings!
                    zlib))

      (home-page "https://weidu.org/")
      (synopsis "InfinityEngine modding tool")
      (description
       "WeiDU (Weimer Dialogue Utility) is a program used to develop, distribute,
and install modifications (mods) for games based on the Infinity Engine.
Supported games include:

@itemize
@item Baldur's Gate and Baldur's Gate II (including Enhanced Editions)
@item Icewind Dale and Icewind Dale II (including Enhanced Editions)
@item Planescape: Torment (including Enhanced Edition)
@end itemize

WeiDU provides a high-level scripting language (WeiDU DSL) for describing game
modifications.  It handles binary patching of game files, manages complex mod
installation dependencies, tracks installation order, and provides automatic
backup and restoration capabilities.

The package includes three command-line programs:

@itemize
@item @command{weidu}: The main modding tool for installing and developing mods
@item @command{weinstall}: Uninstalls previously installed mods
@item @command{tolower}: Converts game filenames to lowercase, required when
running Windows-based Infinity Engine games on case-sensitive filesystems
@end itemize

WeiDU has become the de facto standard for Infinity Engine modding, with
thousands of community mods relying on it.")

      (properties
       `((upstream-name . "WeiDU")))

      (license license:gpl2))))
