(define-module (trisk packages clj-kondo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary))

(define-public clj-kondo
  (package
   (name "clj-kondo")
   (version "2023.05.26")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/clj-kondo/clj-kondo/releases/download/v" version "/clj-kondo-" version "-linux-amd64.zip"))
            (sha256
             (base32
              "1fkksl7jl2vnzkygwgqmjpqsvrf0j94i00dp5y2yhm2kipla68fd"))))
   (build-system binary-build-system)
   (supported-systems '("x86_64-linux" "i686-linux"))
   (arguments
    `(#:patchelf-plan
      `(("clj-kondo"
         ("libc" "zlib" "libstdc++")))
      #:install-plan
      `(("." ("clj-kondo") "bin/"))
      #:phases
      (modify-phases %standard-phases
		     ;; this is required because standard unpack expects
		     ;; the archive to contain a directory with everything inside it,
		     ;; while clj-kondo's release .zip only contains the `clj-kondo` binary.
		     (replace 'unpack
			      (lambda* (#:key inputs #:allow-other-keys)
				(system* (which "unzip")
					 (assoc-ref inputs "source"))
				#t)))))
   (inputs
    `(("libstdc++" ,(make-libstdc++ gcc))
      ("zlib" ,zlib)))
   (native-inputs
    `(("unzip" ,unzip)))
   (synopsis "Static analyzer and linter for Clojure code that sparks joy")
   (description
    "Clj-kondo performs static analysis on Clojure, ClojureScript and EDN,
without the need of a running REPL. It informs you about potential errors
while you are typing.")
   (home-page "https://github.com/clj-kondo/clj-kondo")
   (license license:epl1.0)))
