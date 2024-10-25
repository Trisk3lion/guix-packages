(define-module (trisk packages docker)
  #:use-module (gnu packages docker)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (ice-9 match))

(define-public docker-compose-plugin
  (package
   (inherit docker-compose)
   (version "2.29.7")
   (source
    (let ((arch (match (or (%current-target-system) (%current-system))
                  ("aarch64-linux" "aarch64")
                  ("armhf-linux" "armv7")
                  (_ "x86_64"))))
      (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/docker/compose/releases/download/v"
         version "/docker-compose-linux-" arch))
       (sha256
        (base32
         (match arch
           ("aarch64" "0j392awfb4fh4rrdmzz5gapqam0j96nzky9qi97kh93zkbzr0pjk")
           ("armv7" "1d7g3cmbd0bwyhvq2nww4fbwmrvbfrp6vzwn92r24ys05i68n940")
           (_ "1w5s2agaxi1jkx6l0h6kfx5f752hsyg4ij6jb3wvpmfmiilycg1q")))))))
   (build-system copy-build-system)
   (native-inputs
    (list))
   (inputs
    (list))
   (propagated-inputs
    (list))
   (arguments
    (list
     #:substitutable? #f
     #:install-plan
     #~'(("docker-compose" "libexec/docker/cli-plugins/"))
     #:phases
     #~(modify-phases %standard-phases
                      (replace 'unpack
                               (lambda _
                                 (copy-file #$source "./docker-compose")
                                 (chmod "docker-compose" #o644)))
                      (add-before 'install 'chmod
                                  (lambda _
                                    (chmod "docker-compose" #o555)))
                      (add-after 'install 'setup-bin
                                 (lambda _
                                   (let ((bin (string-append #$output "/bin"))
                                         (lib (string-append #$output "/libexec/docker/cli-plugins")))
                                     (mkdir bin)
                                     (symlink (string-append lib "/docker-compose")
                                              (string-append bin "/docker-compose"))))))))
   (supported-systems '("armhf-linux" "aarch64-linux" "x86_64-linux"))))
