(define-module (trisk packages rexx)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages java)          ; New: For OpenJDK
  #:use-module (gnu packages xml)           ; New: For xsltproc
  #:use-module (gnu packages docbook)       ; New: For DocBook XSL
  #:use-module (gnu packages documentation)) ; New: For Doxygen



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
