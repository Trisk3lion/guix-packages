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
    (version "3.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Syllo/nvtop")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b390h0xnjs3kkbbdw789g7pg3zm3xqh90f622mmmkyravskyy63"))))
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
