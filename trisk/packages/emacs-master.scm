(define-module (trisk packages emacs-master)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (ice-9 regex)
  #:use-module (rnrs lists)
  ;; #:use-module (trisk packages)
  )

(define emacs-git-commit
  "103a2fdf18d99c872e4adb01a15a8300164acd66")
(define emacs-git-hash
  "19w5nrnhj5b26l2pjkq9adsp1kpixq8d334mvsnr1j5by8229i0c")
(define emacs-git-time
  "1712773724")

;; (define-public emacs-master-minimal
;;   (package
;;    (inherit emacs-next-minimal)
;;    (name "emacs-master-minimal")
;;    (version (git-version "30.0.50" emacs-git-time emacs-git-commit))
;;    (source
;;     (origin
;;      (inherit (package-source emacs-next-minimal))
;;      (method url-fetch)
;;      (uri (string-append
;;            "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-"
;;            emacs-git-commit ".tar.gz"))
;;      (sha256
;;       (base32 emacs-git-hash))
;;      (patches ;; (append
;;       ;;  (search-patches "emacs-master-fix-scheme-indent-function.patch")
;;       ;;  (delete (car (search-patches
;;       ;;                "emacs-fix-scheme-indent-function.patch"))
;;       ;;          ;; HACK: I don't know why this patch doesn't work.  :/
;;       ;;          (delete (car (search-patches
;;       ;;                        "emacs-next-exec-path.patch"))
;;       (origin-patches (package-source
;;                        emacs-next-minimal)))))))

;; (define* (emacs->emacs-master emacs
;;                               #:optional name
;;                               #:key (version (package-version
;;                                               emacs-master-minimal))
;;                               (source (package-source emacs-master-minimal)))
;;   (package
;;     (inherit emacs)
;;     (name (or name
;;               (and (string-prefix? "emacs"
;;                                    (package-name emacs))
;;                    (string-append "trisk-emacs-master"
;;                                   (string-drop (package-name emacs)
;;                                                (string-length "emacs"))))))
;;     (version version)
;;     (source
;;      source)))

;; (define-public trisk-emacs-master-no-x
;;   (emacs->emacs-master emacs-no-x))
;; (define-public trisk-emacs-master
;;   (emacs->emacs-master emacs))
