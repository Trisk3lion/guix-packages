(define-module (trisk packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages machine-learning)
  #:use-module (trisk packages binaries))

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

(define-public emacs-whisper
  (let ((commit "6198ce3d9bff0555cf098a77b78d6c2d79baf4f9")
        (revision "0"))
    (package
      (name "emacs-whisper")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/natrys/whisper.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0f72sa92hz0nxq469ajgwjnriwqbqq6snwxqhrzz0izhwmnkmks5"))))
      (build-system emacs-build-system)
      (arguments
       (list
       #:phases
         #~(modify-phases %standard-phases
           (add-after 'unpack 'substitute-whisper-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((whisper-dir (string-append
                                   (assoc-ref inputs "whisper-cpp") "/bin/")))
               (emacs-substitute-variables "whisper.el"
                 ("whisper-install-directory" whisper-dir)
                 ("whisper-install-whispercpp" '(quote manual)))))))))
      (propagated-inputs (list whisper-cpp))
      (home-page "https://github.com/natrys/whisper.el")
      (synopsis "Emacs interface to whisper")
      (description
       "Speech-to-Text interface for Emacs using OpenAI’s whisper speech recognition model. For inference, it uses the C/C++ port whisper.cpp that can run on consumer grade CPU without requiring a high end GPU.

You can capture audio with your input device (microphone) or choose a media file on disk, and have the transcribed text inserted into your Emacs buffer, optionally after translating to English from your local language. This runs offline without having to use non-free cloud services (though quality varies depending on the language).")
      (license license:gpl3+))))

(define-public emacs-editor-code-assistant
  (let ((commit "6a340d318171eeb8d43b9f9ce24a0d90b3a6b59a")
        (revision "0"))
    (package
      (name "emacs-editor-code-assistant")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/editor-code-assistant/eca-emacs")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xrd6vfpv7xlpbmkvm4dflbsxdk0083jfay3mdq3ag2nw6whk7br"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
          #~(modify-phases %standard-phases
           (add-after 'unpack 'substitute-whisper-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((eca (assoc-ref inputs "editor-code-assistant"))
                      (eca-bin (string-append eca "/bin/eca")))
               (emacs-substitute-sexps "eca-process.el"
                 ("(defun eca-process--server-command ()
  \"Return the command to start server.\"
  (let ((system-command" eca-bin))))))))
      (propagated-inputs (list editor-code-assistant
                               emacs-transient
                               emacs-dash
                               emacs-f
                               emacs-markdown-mode
                               emacs-compat))
      (home-page "https://github.com/editor-code-assistant/eca-emacs")
      (synopsis "ECA; Editor Code Assistant for Emacs")
      (description
       "ECA (Editor Code Assistant) Emacs is an AI-powered pair-programming client for Emacs. Inspired by lsp-mode’s JSONRPC handling, it connects to an external eca server process to provide interactive chat, code suggestions, context management and more..")
      (license license:gpl3+))))
