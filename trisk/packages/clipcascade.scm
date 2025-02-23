(define-module (trisk packages clipcascade)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages java))

(define-public clipcascade-server
  (package
    (name "clipcascade-server")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Sathvik-Rao/ClipCascade/releases/download/" version "/ClipCascade-Server-JRE_17.jar"))
              ;; https://github.com/Sathvik-Rao/ClipCascade/releases/download/3.0.0/ClipCascade-Server-JRE_17.jar
              (sha256
               (base32 "11kn3py7r26i5g6jnd3nichz5lq7xskssc27nj0xxjllsxhcvwq9"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'make-wrapper
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (wrapper (string-append out "/bin/clipcascade")))
                        (with-output-to-file wrapper
                          (lambda _
                            (display (string-append "#!/bin/sh\n\n"
                                      (assoc-ref inputs "openjdk")
                                      "/bin/java "
                                      "-jar "
                                      out
                                      "/bin/ClipCascade-Server-JRE_17.jar\n"))))
                        (chmod wrapper #o555)) #t)))
       #:install-plan '(("ClipCascade-Server-JRE_17.jar"
                         "bin/ClipCascade-Server-JRE_17.jar"))))
    (build-system copy-build-system)
    (propagated-inputs (list openjdk))
    (home-page "https://github.com/Sathvik-Rao/ClipCascade")
    (synopsis "ClipCascade is a lightweight utility for the clipboard.")
    (description "ClipCascade is a lightweight utility that automatically syncs the clipboard across devices, no key press required.")
    (license license:expat)))
