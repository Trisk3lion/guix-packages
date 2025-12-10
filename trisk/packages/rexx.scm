(define-module (trisk packages rexx)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages java)           ; New: For OpenJDK
  #:use-module (gnu packages xml)            ; New: For xsltproc
  #:use-module (gnu packages docbook)        ; New: For DocBook XSL
  #:use-module (gnu packages documentation)) ; New: For Doxygen

(define-public brexx
  (package
    (name "brexx")
    (version "2.1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vlachoudis/brexx")
             (commit "master")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15svwjvq5591fd34k5076awikf99ssjgz2m8nicv6wijp7wypr6r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No test suite provided
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; No configure script
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             ;; Use the linux target from make.cnf
             (apply invoke "make" "linux64" make-flags)))
         ;; (replace 'install
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (bin (string-append out "/bin"))
         ;;            (lib (string-append out "/lib/brexx"))
         ;;            (inc (string-append out "/include/brexx"))
         ;;            (share (string-append out "/share/brexx"))
         ;;            (doc (string-append out "/share/doc/brexx")))
         ;;       ;; Create directories
         ;;       (mkdir-p bin)
         ;;       (mkdir-p lib)
         ;;       (mkdir-p inc)
         ;;       (mkdir-p share)
         ;;       (mkdir-p doc)

         ;;       ;; Install the main executable
         ;;       (install-file "src/rexx" bin)

         ;;       ;; Install the lstring library
         ;;       (for-each (lambda (file)
         ;;                  (when (file-exists? file)
         ;;                    (install-file file lib)))
         ;;                '("lstring/liblstring.a"
         ;;                  "lstring/liblstring.so"))

         ;;       ;; Install header files
         ;;       (for-each (lambda (file)
         ;;                  (install-file file inc))
         ;;                (find-files "inc" "\\.h$"))

         ;;       ;; Install REXX library files
         ;;       (for-each (lambda (file)
         ;;                  (install-file file share))
         ;;                (find-files "lib" "\\.r$"))

         ;;       ;; Install programs/examples
         ;;       (when (file-exists? "progs")
         ;;         (copy-recursively "progs"
         ;;                         (string-append share "/examples")))

         ;;       ;; Install documentation
         ;;       (for-each (lambda (file)
         ;;                  (when (file-exists? file)
         ;;                    (install-file file doc)))
         ;;                '("README.md" "AUTHORS" "COPYING"
         ;;                  "ChangeLog" "INSTALL"))

         ;;       ;; Install documentation files
         ;;       (when (file-exists? "doc")
         ;;         (for-each (lambda (file)
         ;;                    (install-file file doc))
         ;;                  (find-files "doc" "\\.(txt|html|htm|pdf)$")))

         ;;       ;; Install shell scripts for environment setup
         ;;       (for-each (lambda (file)
         ;;                  (when (file-exists? file)
         ;;                    (install-file file share)))
         ;;                '("brexx.sh" "brexx.csh"))

         ;;       #t)))
         )))
    (inputs
     (list readline ncurses))
    (synopsis "REXX programming language interpreter")
    (description
     "BRexx is a classic REXX interpreter implementation.  REXX is a procedural
programming language designed by Michael Cowlishaw of IBM UK Laboratories that
allows programs and algorithms to be written in a clear and structured way.

BRexx is written in ANSI C and is designed to be fast and flexible, with very
little restrictions, while maintaining compatibility with ANSI-REXX.  REXX is
particularly well-suited as a macro language for arbitrary applications, as it
provides a powerful string handling library, a versatile PARSE command, and the
ability to execute system commands and use their output.

Key features include:
@itemize
@item Simple and consistent syntax
@item Dynamic typing - all variables are strings
@item No variable declaration required
@item Unlimited variable sizes (memory permitting)
@item Extensive string manipulation functions
@item Mathematical library with scientific functions
@item File I/O routines
@item Built-in stack for communication with the OS
@item Support for creating REXX libraries
@item Interactive tracing and debugging support
@end itemize

This interpreter can be used standalone or as a macro language for applications.")
    (home-page "https://github.com/vlachoudis/brexx")
    (license license:lgpl2.1+)))

(define-public oorexx
  (package
    (name "oorexx")
    (version "5.1.0-12973")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/" name "/" name "/"
                           "5.1.0/oorexx-" version ".tar.gz"))
       (sha256
        (base32 "0ym6qsjhsi9da7y195ahr76rm2f3ffahxgbvkd8kk643r5kwchv3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DCMAKE_BUILD_TYPE=Release"
             ;; Explicitly enable documentation if the build script supports the flag,
             ;; otherwise detection of xsltproc/doxygen usually triggers it.
             "-DBUILD_DOCS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-java-home
           ;; Sometimes CMake needs a hint where the JDK is, even if in environment.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
             #t)))))
    (inputs (list ncurses
                  openjdk)) ; Java dependency for JNI bindings
    (native-inputs (list doxygen       ; Generates API documentation
                         libxslt       ; Contains xsltproc for processing DocBook
                         docbook-xsl)) ; Stylesheets for documentation
    (home-page "http://www.oorexx.org/")
    (synopsis "Open Object Rexx Interpreter with Java bindings")
    (description
     "Open Object Rexx (ooRexx) is an Open Source project managed by the
Rexx Language Association. It provides a backward-compatible version of
classic Rexx with object-oriented extensions.

This package includes:
* The core Interpreter (rexx)
* The API daemon (rxapi)
* Java Native Interface (JNI) bindings for Java integration.")
    (license license:cpl1.0)))

(define-public regina-rexx
  (package
    (name "regina-rexx")
    (version "3.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/regina-rexx/regina-rexx/"
                           version "/regina-rexx-" version ".tar.gz"))
       (sha256
        (base32 "0ldw58xfgi3f8i16czx3xr8axzbgfqhdwijlnk7kh07f3c3aks88"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; Makefiles sometimes have race conditions
       #:tests? #f ; Tests usually require an interactive shell or specific setup
       #:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out"))
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))))
    (home-page "http://regina-rexx.sourceforge.net/")
    (synopsis "Regina Rexx Interpreter")
    (description
     "Regina is a Rexx interpreter that has been ported to most Unix platforms
and other operating systems. Rexx is a procedural scripting language designed
to be easy to learn and read.")
    (license license:lgpl2.1+)))
