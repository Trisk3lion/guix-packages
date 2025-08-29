(define-module (trisk packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  )


(define (tree-sitter-delete-generated-files grammar-directories)
  #~(begin
      (use-modules (guix build utils))
      (delete-file "binding.gyp")
      (delete-file-recursively "bindings")
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '#$grammar-directories)))

(define* (tree-sitter-grammar
          name text hash version
          #:key
          (commit (string-append "v" version))
          (repository-url
           (format #f "https://github.com/tree-sitter/tree-sitter-~a" name))
          (grammar-directories '("."))
          (article "a")
          (inputs '())
          (get-cleanup-snippet tree-sitter-delete-generated-files)
          (license license:expat))
  "Returns a package for Tree-sitter grammar.  NAME will be used with
tree-sitter- prefix to generate package name and also for generating
REPOSITORY-URL value if it's not specified explicitly, TEXT is a string which
will be used in description and synopsis. GET-CLEANUP-SNIPPET is a function,
it receives GRAMMAR-DIRECTORIES as an argument and should return a G-exp,
which will be used as a snippet in origin."
  (let* ((multiple? (> (length grammar-directories) 1))
         (grammar-names (string-append text " grammar" (if multiple? "s" "")))
         (synopsis (string-append "Tree-sitter " grammar-names))
         (description
          (string-append "This package provides "
                         (if multiple? "" article) (if multiple? "" " ")
                         grammar-names " for the Tree-sitter library."))
         (name (string-append "tree-sitter-" name)))
    (package
      (name name)
      (version version)
      (home-page repository-url)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url repository-url)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))
                (snippet
                 (get-cleanup-snippet grammar-directories))))
      (build-system tree-sitter-build-system)
      (arguments (list #:grammar-directories grammar-directories))
      (inputs inputs)
      (synopsis synopsis)
      (description description)
      (license license))))




(define-public tree-sitter-janet-simple
  (let ((version "0.0.7") ; In package.json, but untagged
        (commit "7e28cbf1ca061887ea43591a2898001f4245fddf")
        (revision "0"))
  (tree-sitter-grammar
   "janet-simple" "Janet-simple"
   "05xn760xxax8q2dsvyj3yw90rhnq7nmv54p0i8af34nhjwyi8sx9"
   (git-version version revision commit)
   #:repository-url "https://github.com/sogaiu/tree-sitter-janet-simple"
   #:license license:expat
   #:commit commit
   #:get-cleanup-snippet
   (lambda _
     #~(begin
      (use-modules (guix build utils))
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '(".")))))))
