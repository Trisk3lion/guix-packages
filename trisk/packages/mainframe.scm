(define-module (trisk packages mainframe)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression))

(define-public hercules
  (package
    (name "hercules")
    (version "3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://downloads.hercules-390.eu/hercules-"
                           version ".tar.gz"))
       (sha256
        (base32 "0zg6rwz8ib4alibf8lygi8qn69xx8n92kbi8b3jhi1ymb32mf349"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-cckd-bzip2" "--enable-het-bzip2"
                               "--enable-multi-cpu=128")))
    (inputs `(("bzip2" ,bzip2)
              ("zlib" ,zlib)))
    (home-page "http://www.hercules-390.eu/")
    (synopsis "System/370, ESA/390 and z/Architecture Emulator")
    (description
     "Hercules is an open source software implementation of the mainframe
System/370 and ESA/390 architectures, in addition to the new 64-bit
z/Architecture.
This means that your PC can emulate an IBM mainframe processor.  The mainframe
can range from a 360 to a z900 - running in \"System/370\" mode, \"ESA/390\"
mode, or \"z/Architecture\" mode.  Hercules executes S/370, ESA/390, and
z/Architecture instructions and channel programs.  It emulates mainframe I/O
devices by using PC devices.  For example, 3390 DASD devices are emulated by
large files on your hard disk, and local 3270 screens are emulated by tn3270
sessions.
Hercules implements only the raw S/370, ESA/390, and z/Architecture instruction
set; it does not provide any operating system facilities.  This means that you
need to provide an operating system or standalone program which Hercules can
load from an emulated disk or tape device.  You will have to use a free software
operating system such as Linux, write the operating system or standalone program
yourself, obtain a license from IBM to run one of their operating systems on
your PC, or use IBM programs and operating systems which have been placed in the
public domain.
Virtual networking can be accomplished using the TUN/TAP driver in host Linux
kernel.")
    (license license:qpl)))

;; TODO sdl-hercules
;; TODO athena-hercules
