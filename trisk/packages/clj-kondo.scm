(define-module (trisk packages clj-kondo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system copy))


(define clj-kondo-git-version "2024.03.13")
(define clj-kondo-git-hash "1c78i4wx4w1r6d5mwgnvcnnq3pqnm13d77y3ann0n8dw30n1akz1")

;; Has been upstreamed to nongnu guix
;; (define-public clj-kondo
;;   (package
;;    (name "clj-kondo")
;;    (version clj-kondo-git-version)
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://github.com/clj-kondo/clj-kondo/releases/download/v" version "/clj-kondo-" version "-linux-static-amd64.zip"))
;;             (sha256
;;              (base32 clj-kondo-git-hash))))
;;    (build-system copy-build-system)
;;    (supported-systems '("x86_64-linux" "i686-linux"))
;;    (arguments
;;     `(#:install-plan '(("clj-kondo" "bin/"))))
;;    ;; (arguments
;;    ;;  `(#:patchelf-plan
;;    ;;    `(("clj-kondo"
;;    ;;       ("libc" "zlib" "libstdc++")))
;;    ;;    #:install-plan
;;    ;;    `(("." ("clj-kondo") "bin/"))
;;    ;;    #:phases
;;    ;;    (modify-phases %standard-phases
;;    ;;      	     ;; this is required because standard unpack expects
;;    ;;      	     ;; the archive to contain a directory with everything inside it,
;;    ;;      	     ;; while clj-kondo's release .zip only contains the `clj-kondo` binary.
;;    ;;      	     (replace 'unpack
;;    ;;      		      (lambda* (#:key inputs #:allow-other-keys)
;;    ;;      			(system* (which "unzip")
;;    ;;      				 (assoc-ref inputs "source"))
;;    ;;      			#t)))))
;;    (native-inputs (list unzip))
;;    (synopsis "Static analyzer and linter for Clojure code that sparks joy")
;;    (description
;;     "Clj-kondo performs static analysis on Clojure, ClojureScript and EDN,
;; without the need of a running REPL. It informs you about potential errors
;; while you are typing.")
;;    (home-page "https://github.com/clj-kondo/clj-kondo")
;;    (license license:epl1.0)))
