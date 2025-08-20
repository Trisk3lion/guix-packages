(define-module (trisk packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build))

(define-public emacs-transient-latest
  (package
    (inherit emacs-transient)
    (name "emacs-transient-latest")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/magit/transient")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08szibb4h1x8d9x2nacwa71z32mxr4wjgw8g6j4yxj2fs61ilj63"))))))

(define-public emacs-claude-code-ide
  (let ((commit "907f28ed82b743b0fff945ebe772d10953d78bfe")
        (revision "0"))
    (package
      (name "emacs-claude-code-ide")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/manzaltu/claude-code-ide.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zhbwdsy83514afxg81aywgl72sjnc5w5aibryyd44kp45qbf5wx"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs (list emacs-vterm emacs-websocket emacs-transient emacs-flycheck))
      (home-page "https://github.com/manzaltu/claude-code-ide.el")
      (synopsis "Claude Code in Emacs")
      (description
       "This package provides an Emacs library to manage vterm buffers.")
      (license license:gpl3+))))

(define-public emacs-claude-code
  (let ((commit "b4c14ea23bcec456a6b9fed3aedeee3f2a2c6aae")
        (revision "0"))
    (package
      (name "emacs-claude-code")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stevemolitor/claude-code.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jgs04m37dpw6lj2wkjlmbfgi3cyfq2dkivv5ap12zrrb1zyj9g1"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs (list emacs-transient))
      (home-page "https://github.com/manzaltu/claude-code-ide.el")
      (synopsis "Claude Code in Emacs")
      (description
       "This package provides an Emacs library to manage vterm buffers.")
      (license license:gpl3+))))
