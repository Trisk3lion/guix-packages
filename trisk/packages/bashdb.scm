(define-module (trisk packages bashdb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build))

(define-public bashdb
  (package
    (name "bashdb")
    (version "5.2-1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Trepan-Debuggers/bashdb"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "024brbys6kgckb4przpxg9d582x2zwzdz2gpydjazkacp3kkf4sc"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove unnecessary files and fix bash version compatibility
               #~(begin
                   ;; Remove HTML documentation to reduce size
                   (delete-file-recursively "htdocs")
                   ;; Fix bash version check to accept any bash 5.x
                   (substitute* "configure.ac"
                     (("case \"\\$\\{bash_major\\}\\.\\$\\{bash_minor\\}\" in")
                      "case \"${bash_major}\" in")
                     (("'OK_BASH_VERS' \\| '5\\.0' \\| '5\\.1'\\)")
                      "'5')")
                     (("This package is only known to work with Bash 5\\.0 or 5\\.1")
                      "This package is only known to work with Bash 5"))
                   #t))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Tests require TMPDIR with exec permissions, which is not available
      ;; in the Guix build environment
      #:tests? #f  
      #:configure-flags
      #~(list (string-append "--with-dbg-main=" #$output
                             "/share/bashdb/bashdb-main.inc"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-python-shebang
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Fix python shebang in term-highlight.py
              (substitute* "lib/term-highlight.py"
                (("#!/usr/bin/env python")
                 (string-append "#!" (which "python3"))))
              #t))
          (add-before 'configure 'generate-configure
            (lambda _
              ;; Generate configure script from configure.ac
              (invoke "autoreconf" "-vfi")
              #t)))))
    (native-inputs
     (list autoconf
           automake
           libtool
           texinfo  ; for makeinfo to generate documentation
           perl))   ; for pod2man to generate man pages
    (inputs
     (list python-wrapper    ; for the term-highlight.py script
           python-pygments)) ; for syntax highlighting support
    (home-page "https://bashdb.sourceforge.net/")
    (synopsis "GDB-like debugger for Bash")
    (description
     "The Bash Debugger Project is a source-code debugger for Bash that
follows the GDB command syntax.  It provides features like setting breakpoints,
stepping through code, examining variables, and call stack inspection for Bash
scripts.  It supports Bash version 5.2 and higher.

Key features include:
@itemize
@item Setting and managing breakpoints
@item Stepping through code line by line
@item Examining and modifying variables
@item Call stack inspection and navigation  
@item Syntax highlighting support
@item Command completion
@item Integration with editors
@end itemize

The debugger can be invoked in several ways: through @code{bash --debugger},
directly with @code{bashdb}, or by sourcing the trace script within a Bash
script.")
    (license license:gpl2+)))
