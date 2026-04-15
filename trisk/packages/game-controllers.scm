(define-module (trisk packages game-controllers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial))

(define* (xone-dongle-firmware #:key pid fwname url hash)
  (hidden-package
   (package
     (name (string-append "xone-dongle-" pid))
     (version "0-unstable-2025-12-18")
     (source
      (origin
        (method url-fetch)
        (uri url)
        (sha256
         (base32 hash))))
     (build-system trivial-build-system)
     (arguments
      (list
       #:modules '((guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build utils))
           (let ((PATH (string-append (assoc-ref %build-inputs "cabextract") "/bin"))
                 (source (assoc-ref %build-inputs "source"))
                 (firmware-dir (string-append #$output "/lib/firmware")))
             (setenv "PATH" PATH)
             (system* "cabextract" "-F" #$fwname source)
             (mkdir-p firmware-dir)
             (copy-file #$fwname (string-append firmware-dir "/xone_dongle_" #$pid ".bin"))))))
     (native-inputs (list cabextract))
     (synopsis "Xbox One wireless dongle firmware")
     (description "Xbox One wireless dongle firmware")
     (home-page "https://www.microsoft.com/en-us/d/xbox-wireless-adapter-for-windows/91dqrb97l130")
     (license (nonfree #f)))))

(define-public xone-dongle-02e6
  (xone-dongle-firmware
   #:pid "02e6"
   #:fwname "FW_ACC_00U.bin"
   #:url "https://catalog.s.download.windowsupdate.com/d/msdownload/update/driver/drvs/2017/03/2ea9591b-f751-442c-80ce-8f4692cdc67b_6b555a3a288153cf04aec6e03cba360afe2fce34.cab"
   #:hash "0cpgb0i4dnfm0h3kc7xc0lhc4d2cypkpz22wdpqw9dqhvkl756nq"))

(define-public xone-dongle-02fe
  (xone-dongle-firmware
   #:pid "02fe"
   #:fwname "FW_ACC_00U.bin"
   #:url "https://catalog.s.download.windowsupdate.com/c/msdownload/update/driver/drvs/2017/07/1cd6a87c-623f-4407-a52d-c31be49e925c_e19f60808bdcbfbd3c3df6be3e71ffc52e43261e.cab"
   #:hash "013g1zngxffavqrk5jy934q3bdhsv6z05ilfixdn8dj0zy26lwv5"))

(define-public xone-dongle-02f9
  (xone-dongle-firmware
   #:pid "02f9"
   #:fwname "FW_ACC_CL.bin"
   #:url "https://catalog.s.download.windowsupdate.com/c/msdownload/update/driver/drvs/2017/06/1dbd7cb4-53bc-4857-a5b0-5955c8acaf71_9081931e7d664429a93ffda0db41b7545b7ac257.cab"
   #:hash "1q1fmng898aqp0nzdq4vcm5qzwfhwz00k0gx0xs3h3a6czxr3pch"))

(define-public xone-dongle-091e
  (xone-dongle-firmware
   #:pid "091e"
   #:fwname "FW_ACC_BR.bin"
   #:url "https://catalog.s.download.windowsupdate.com/d/msdownload/update/driver/drvs/2017/08/aeff215c-3bc4-4d36-a3ea-e14bfa8fa9d2_e58550c4f74a27e51e5cb6868b10ff633fa77164.cab"
   #:hash "1wnqrh130hxyi0ddjq9d0ac30rwplh674d47g9lwqn0yabcvm3ss"))


(define-public xone-dongle-firmware
  (package
    (name "xone-dongle-firmware")
    (version "0-unstable-2025-12-18")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (inputs (list xone-dongle-02e6
                  xone-dongle-02f9
                  xone-dongle-02fe
                  xone-dongle-091e))
    (home-page "https://www.microsoft.com/en-us/d/xbox-wireless-adapter-for-windows/91dqrb97l130")
    (synopsis "Xbox One wireless dongle firmware")
    (description "Xbox One wireless dongle firmware.")
    (license (nonfree #f))))

(define-public xone
  (package
    (name "xone")
    (version "0.5.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dlundqvist/xone")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xfmnv3xk03dazzckz6zd2svysrvbc3xq38wcbbvqp9kwm43zn8h"))))
    (build-system linux-module-build-system)
    (arguments
     '(#:tests? #f)) ;no tests
    (synopsis "Linux kernel module for the xone firmware controller")
    (description
     "The reform2_lpc module allows for interaction with the NXP LPC11U24
Cortex-M0 MCU system controller in the Reform 2 open hardware laptop.  It
provides battery status information and is necessary to completely shut down
the system when powering it off via userspace.")
    (home-page "https://source.mnt.re/reform/reform-tools/")
    (license license:gpl3+)))

(define-public xpad-noone
  (let ((commit "7b5361475c719b03b9e0152e44cf7fca0b85af60")
        (revision "0"))
    (package
      (name "xpad-noone")
      (version (git-version "0.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/forkymcforkface/xpad-noone")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0vzjnrji9vzsa3agdjw0pf06qvbn1dk1ad2lphvkfh74586kvr6x"))))
      (build-system linux-module-build-system)
      (arguments
       '(#:tests? #f)) ;no tests
      (propagated-inputs (list curl))
      (synopsis "Linux kernel module for the Reform 2 system controller")
      (description
       "The reform2_lpc module allows for interaction with the NXP LPC11U24
Cortex-M0 MCU system controller in the Reform 2 open hardware laptop.  It
provides battery status information and is necessary to completely shut down
the system when powering it off via userspace.")
      (home-page "https://source.mnt.re/reform/reform-tools/")
      (license license:gpl3+))))
