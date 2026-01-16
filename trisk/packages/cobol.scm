(define-module (trisk packages cobol)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:))

(define-public che-lsp-for-cobol
  (package
    (name "che-lsp-for-cobol")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol/releases/download/"
             version "/cobol-language-support-linux-x64-" version ".vsix"))
       (sha256
        (base32 "05krf1rlmi79y7cmi6xcayv13160zsz1dd84xnlw5597gpbm9qkp"))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source)))
                  (add-after 'install 'make-wrapper
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (wrapper (string-append out "/bin/cobol-lsp-java")))
                        (with-output-to-file wrapper
                          (lambda _
                            (display (string-append "#!/bin/sh\n\n"
                                                    (assoc-ref inputs "openjdk")
                                                    "/bin/java "
                                                    "-Dline.speparator=\\r\\n "
                                                    "-Xmx768M "
                                                    "-jar "
                                                    out
                                                    "/share/java/server.jar pipeEnabled\n"))))
                        (chmod wrapper #o555)) #t)))
       #:install-plan '(("extension/server/native/server-linux"
                         "bin/cobol-lsp-native")
                        ("extension/server/jar/server.jar"
                         "share/java/server.jar"))))
    (native-inputs (list unzip))
    (propagated-inputs (list openjdk))
    (home-page "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol")
    (synopsis
     "COBOL Language Support provides autocomplete, highlighting and diagnostics for COBOL code and copybooks.")
    (description
     "COBOL Language Support enhances the COBOL programming experience on your IDE. The extension leverages the language server protocol to provide autocomplete, syntax highlighting and coloring, and diagnostic features for COBOL code and copybooks. The COBOL Language Support extension can also connect to a mainframe using the Zowe Explorer extension to automatically retrieve copybooks used in your programs and store them in your workspace. COBOL Language Support also supports COBOL programs which interact with Datacom, CICS, and DB2 SQL. An add-on extension which adds support for the IDMS dialect is available on the VS Code Marketplace.")
    (license license:expat)))
