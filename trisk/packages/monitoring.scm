(define-module (trisk packages monitoring)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (nongnu packages nvidia))

(define-public nvtop
  (package
    (name "nvtop")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Syllo/nvtop")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "006gqk8jdmllbc84azmi113dc0xb2v9vyahkk5kmds6icdih2j9j"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DNVIDIA_SUPPORT=ON"
                                     "-DAMDGPU_SUPPORT=ON"
                                     "-DINTEL_SUPPORT=ON"
                                     "-DUSE_LIBUDEV_OVER_LIBSYSTEMD=ON"
                                     "-DBUILD_TESTING=OFF")
           #:build-type "Release"
           #:tests? #f))
    (inputs (list ncurses nvidia-nvml libdrm eudev))
    (home-page "https://github.com/Syllo/nvtop")
    (synopsis "GPUs process monitoring for AMD, Intel and NVIDIA ")
    (description "Nvtop stands for Neat Videocard TOP,
 a (h)top like task monitor for AMD, Intel and NVIDIA GPUs.
 It can handle multiple GPUs and print information
 about them in a htop familiar way.")
    (license license:gpl3)))
