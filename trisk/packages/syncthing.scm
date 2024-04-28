(define-module (trisk packages syncthing)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages syncthing))

(define-public syncthing-goamd64v3
  (package
    (inherit syncthing)
    (name "syncthing-goamd64v3") ;To make it easy to search for.
    (arguments
     (substitute-keyword-arguments (package-arguments syncthing)
       ((#:tests? _ #t)
        #f)
       ((#:phases phases
         '%standard-phases)
        `(modify-phases ,phases
           (add-after 'setup-go-environment 'set-microarchitecture
             (lambda _
               (setenv "GOAMD64" "v3")))))))
    ;; This is the only architecture which can build this package.
    ;; go: cannot install cross-compiled binaries when GOBIN is set
    (supported-systems '("x86_64-linux"))))

(define-public syncthing-goarm5
  (package
    (inherit syncthing)
    (name "syncthing-armhf") ;To make it easy to search for.
    (arguments
     (substitute-keyword-arguments (package-arguments syncthing)
       ((#:phases phases
         '%standard-phases)
        `(modify-phases ,phases
           (add-after 'setup-go-environment 'set-microarchitecture
             (lambda _
               (setenv "GOARM" "5")))))))
    ;; This is the only architecture which can build this package.
    ;; go: cannot install cross-compiled binaries when GOBIN is set
    (supported-systems '("armhf-linux"))))
