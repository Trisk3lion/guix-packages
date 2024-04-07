((nil
  (fill-column . 78)
  (tab-width   .  8)
  (sentence-end-double-space . t)
  (compile-command . "guix build -L . ")
  ;; (geiser-guile-binary  . ("guix" "repl"))
  )

 ;; Emacs-Guix
 (eval . (setq-local guix-directory (expand-file-name "~/src/guix/")))

 (scheme-mode
  (indent-tabs-mode . nil)

  ;; This notably allows '(' in Paredit to not insert a space when the
  ;; preceding symbol is one of these.
  (eval . (modify-syntax-entry ?~ "'"))
  (eval . (modify-syntax-entry ?$ "'"))
  (eval . (modify-syntax-entry ?+ "'"))

  (eval . (with-eval-after-load 'geiser-guile
            (let ((root-dir
                   (file-name-directory
                    (locate-dominating-file default-directory ".dir-locals.el"))))
              (unless (member root-dir geiser-guile-load-path)
                (setq-local geiser-guile-load-path
                            (cons root-dir geiser-guile-load-path))))))))
