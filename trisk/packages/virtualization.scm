(define-module (trisk packages virtualization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages virtualization)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (quickemu))

(define quickemu
  (package
    (name "quickemu")
    (version "4.9.7")
    (source (origin
              (method git-fetch)
              (uri
                (git-reference
                  (url "https://github.com/quickemu-project/quickemu")
                  (commit version)))
              (sha256
                (base32 "05z8h3xrzc5w0pyrhzjhrr56ik9dx316dlbnvx3lmrxxvrq04amh"))))
    (build-system copy-build-system)
    (arguments
      `(#:install-plan '(("quickemu" "bin/")
                         ("quickget" "bin/")
                         ("quickreport" "bin/")
                         ("chunkcheck" "bin/")
                         ("docs/quickget.1" "share/man/man1/")
                         ("docs/quickemu.1" "share/man/man1/"))
        #:phases
        ,#~(modify-phases %standard-phases
             (add-before 'install 'fix-ovmf
               (lambda _
                 (substitute* "quickemu"
                   (("\"\\$\\{SHARE_PATH\\}/edk2/x64/OVMF_CODE.4m.fd\",\"\\$\\{SHARE_PATH\\}/edk2/x64/OVMF_VARS.4m.fd\"")
                    (format #f
                            "~{\"~a\"~^,~}"
                            (list (string-append #$(this-package-input "ovmf-x86-64") "/share/firmware/ovmf_code_x64.bin")
                                  (string-append #$(this-package-input "ovmf-x86-64") "/share/firmware/ovmf_vars_x64.bin"))))
                   (("cp \"\\$\\{VARS_IN\\}\" \"\\$\\{VARS_OUT\\}\"" s)
                    (string-append s " && chmod +w \"${VARS_OUT}\""))))))))
    (inputs (list bash-minimal
                  ovmf-x86-64))
    (propagated-inputs
      (list qemu
            cdrtools
            edk2-tools
            spice-gtk))
    (home-page "https://github.com/quickemu-project/quickemu")
    (synopsis "Quickly create and run optimised Windows, macOS and Linux virtual
machines")
    (description "This package provides a wrapper for the excellent QEMU
that automatically does the right thing when creating virtual machines. No
requirement for exhaustive configuration options. You decide what operating
system you want to run and Quickemu takes care of the rest.")
    (license license:expat)))
