(define-module (trisk utils go-fetch-vendored)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (guix git-download)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu services shepherd)
  #:export (go-git-reference
            go-url-reference
            go-fetch-vendored))

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url    go-git-reference-url)
  (commit go-git-reference-commit)
  (sha    go-git-reference-sha256))

(define-record-type* <go-url-reference>
  go-url-reference make-go-url-reference
  go-url-reference?
  (url go-url-reference-url)
  (sha go-url-reference-sha))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
         (match uri
           (($ <go-git-reference> url commit sha)
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url url)
                    (commit commit)))
              (sha256 sha)))
           (($ <go-url-reference> url commit sha)
            (origin
              (method url-fetch)
              (uri url)
              (sha256 sha)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
     (string-append name "-vendored.tar.gz")
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (let ((inputs (list
                          #+go-1.23
                          #+tar
                          #+bzip2
                          #+gzip)))
             (set-path-environment-variable "PATH" '("/bin") inputs))
           (mkdir "source")
           (chdir "source")
           (if (file-is-directory? #$src) ;; this is taken (lightly edited) from unpack in gnu-build-system.scm
               (begin
                 ;; Preserve timestamps (set to the Epoch) on the copied tree so that
                 ;; things work deterministically.
                 (copy-recursively #$src "."
                                   #:keep-mtime? #t)
                 ;; Make the source checkout files writable, for convenience.
                 (for-each (lambda (f)
                             (false-if-exception (make-file-writable f)))
                           (find-files ".")))
               (begin
                 (cond
                  ((string-suffix? ".zip" #$src)
                   (invoke "unzip" #$src))
                  ((tarball? #$src)
                   (invoke "tar" "xvf" #$src))
                  (else
                   (let ((name (strip-store-file-name #$src))
                         (command (compressor #$src)))
                     (copy-file #$src name)
                     (when command
                       (invoke command "--decompress" name)))))))

           (setenv "GOCACHE" "/tmp/gc")
           (setenv "GOMODCACHE" "/tmp/gmc")
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

           (invoke "go" "mod" "vendor")

           (invoke "tar" "czvf" #$output
                   ;; Avoid non-determinism in the archive.
                   "--mtime=@0"
                   "--owner=root:0"
                   "--group=root:0"
                   "--sort=name"
                   "--hard-dereference"
                   ".")))
     #:hash hash-value
     #:hash-algo hash-algorithm)))
