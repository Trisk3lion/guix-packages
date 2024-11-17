(define-module (trisk packages audio)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls))

(define-public squeezelite
  (package
    (name "squeezelite")
    (version "2.0.0.1488")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ralph-irving/squeezelite")
                    (commit "0e85ddfd79337cdc30b7d29922b1d790600bb6b4")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0ys7y8g9ji6spdcp7k43h6fyms2iv1xldqhgscsds97qrvyshshl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                      ; No check target.
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target)))
      #:configure-flags #~(list "-DLINKALL" "-DGPIO" "-DFFMPEG" "-DUSE_SSL" "-DDSD" "-DRESAMPLE"
                                "-DOPUS"
                                )
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs parallel-build? configure-flags
                      #:allow-other-keys)
              (setenv "OPTS" (string-join configure-flags))))
          (add-before 'build 'adjust-opus
            (lambda _
              (substitute* "opus.c"
                (("<opusfile.h>") "<opus/opusfile.h>"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out")))
                (install-file #$name (string-append out "/bin"))
                ;; (copy-recursively "*.[txt|md]" (string-append out "/share/doc/squeezelite"))
                )
              #t)))))
    (native-inputs (list flac libmad libvorbis mpg123 alsa-lib faad2 ffmpeg opusfile openssl soxr))
    (home-page "https://github.com/ralph-irving/squeezelite")
    (synopsis "Lightweight headless squeezebox client emulator")
    (description "")
    (license license:expat)))

(define-public squeezelite-pulse
  (package
    (inherit squeezelite)
    (name "squeezelite-pulse")
    (arguments
     (substitute-keyword-arguments (package-arguments squeezelite)
       ((#:configure-flags flags) #~(cons* "-DPULSEAUDIO" #$flags)))
       ;; ((#:phases phases) #~(modify-phases #$phases
       ;;                       (replace 'install
       ;;                         (lambda* (#:key outputs #:allow-other-keys)
       ;;                           (let ((out (assoc-ref outputs "out")))
       ;;                             (install-file "squeezelite" (string-append out "/bin/" #$name)))
       ;;                           #t))))
       )
    (native-inputs (list flac libmad libvorbis mpg123 alsa-lib faad2 ffmpeg opusfile openssl soxr pulseaudio))))
