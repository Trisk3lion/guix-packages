(define-module (trisk packages babashka)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary))

(define babashka-git-version "1.3.186")
(define babashka-git-hash "039ak593vmqrw3yhglfdh8rmvwj2aznwqfy6yvfca1wlk1827c3l")

(define-public babashka
  (package
   (name "babashka")
   (version babashka-git-version)
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "https://github.com/babashka/babashka/releases/download/v"
              version "/babashka-" version "-linux-amd64.tar.gz"))
            (sha256
             (base32 babashka-git-hash))))
   (build-system binary-build-system)
   (supported-systems '("x86_64-linux" "i686-linux"))
   (arguments
    `(#:patchelf-plan
      `(("bb"
         ("libc" "zlib" "libstdc++")))
      #:install-plan
      `(("." ("bb") "bin/"))
      #:phases
      (modify-phases
       %standard-phases
       ;; this is required because standard unpack expects the archive to
       ;; contain a directory with everything inside it, while babashka's
       ;; release file only contains the `bb` binary.
       (replace 'unpack
                (lambda* (#:key inputs outputs source #:allow-other-keys)
                  (invoke "tar" "xf" source)
                  #t)))))
   (inputs
    `(("libstdc++" ,(make-libstdc++ gcc))
      ("zlib" ,zlib)))
   (synopsis
    "Fast native Clojure scripting runtime")
   (description
    "Babashka is a native Clojure interpreter for scripting with fast startup.
Its main goal is to leverage Clojure in places where you would be using bash otherwise.")
   (home-page "https://babashka.org/")
   (license license:epl1.0)))
