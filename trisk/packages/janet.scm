(define-module (trisk packages janet)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages lisp))

(define-public janet-jpm
  (let ((commit "d93b7c243645d31410a81fb9ab8f7a5e5608f0d0"))
    (package
      (name "janet-jpm")
      (version (string-append "1.1.0+" (substring commit 0 10)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/janet-lang/jpm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1y15j94p8c0vc7q2xm8li7v9qr0z17xc39n3krxpljwjx6pvlyvm"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                              (delete 'configure)
                              (delete 'install)
                 ;; Remove ':hardcode-syspath' from the 'jpm' script so that it
                 ;; doesn't attempt to hardcode an incorrect JANET_PATH.
                 (add-before 'build 'fix-jpm-lib-path
                   (lambda* (#:key outputs inputs #:allow-other-keys)
                     (substitute* "project.janet"
                       ((":hardcode-syspath true")
                        ""))
                     (substitute* "jpm/make-config.janet"
                       (("\"cc\"")
                        "\"gcc\""))))
                 (add-after 'build 'wrap-jpm
                   (lambda* (#:key inputs #:allow-other-keys)
                     (wrap-program (string-append #$output "/bin/jpm")
                       `("JANET_PATH" = (,(string-append #$output "/lib/janet")))
                       `("JANET_BINPATH" = (,(string-append #$output "/bin")))
                       ;`("JANET_TREE" = (,(string-append #$output "/lib/janet")))
                       `("JANET_LIBPATH" = (,(string-append (assoc-ref inputs "janet") "/lib")))
                       `("JANET_HEADERPATH" = (,(string-append (assoc-ref inputs "janet") "/include"))))))
                 (replace 'build
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((janet-path (assoc-ref inputs "janet")))
                       (setenv "DESTDIR" #$output)
                       (setenv "PREFIX" "")
                       (setenv "CC" #$(cc-for-target))
                       ;(setenv "JANET_LIBPATH" (string-append #$output "/lib"))
                       ;(setenv "JANET_HEADERPATH" (string-append janet-path "/include/janet"))
                       (invoke "janet" "bootstrap.janet")))))))
      (inputs (list janet))
      (home-page "https://janet-lang.org/")
      (synopsis "Janet Project Manager")
           (description
       "Janet is a functional and imperative programming language.  It can be
used for rapid prototyping, dynamic systems, and other domains where dynamic
languages shine.  You can also add Janet scripting to an application by
embedding a single C file and two headers.  It can be easily ported to new
platforms.  The entire language (core library, interpreter, compiler,
assembler, PEG) is less than 1MB.")
      (license license:expat))))

(define-public janet-spork
  (let ((commit "09ad64873b75c386f3abfd1e41d44b990d67487f"))
    (package
      (name "janet-spork")
      (version (string-append "0.0.0+" (substring commit 0 10)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/janet-lang/spork")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bcbga9fk78qbaspl50m4bjc72z45cgj446v3yyjz4vr351j8m19"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (replace 'build
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((janet-path (assoc-ref inputs "janet")))
                       (setenv "CC" #$(cc-for-target))
                       (invoke "jpm" "build"))))
                 (replace 'install
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((janet-tree-path #$output))
                       (mkdir-p janet-tree-path)
                       (setenv "JANET_TREE" janet-tree-path)
                       (invoke "jpm" "install")))))))

      (inputs (list janet janet-jpm))
      (home-page "https://janet-lang.org/")
      (synopsis "Janet Project Manager")
      (description
       "Janet is a functional and imperative programming language.  It can be
used for rapid prototyping, dynamic systems, and other domains where dynamic
languages shine.  You can also add Janet scripting to an application by
embedding a single C file and two headers.  It can be easily ported to new
platforms.  The entire language (core library, interpreter, compiler,
assembler, PEG) is less than 1MB.")
      (license license:expat))))
