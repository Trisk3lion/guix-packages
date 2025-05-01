(define-module (trisk packages python)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python))

(define-public python-basilisp
  (package
    (name "python-basilisp")
    (version "0.3.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/basilisp-lang/basilisp")
             (commit (string-append "v" version))))
        (file-name (git-file-name name version))
       (sha256
        (base32 "1hrvnh049q6yvr4rjzs80sl6qb904im6znbl07hqjp34yb6whfiz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
      ;; #:test-flags #~(list "-k" "not test_sys_executable")))
    (propagated-inputs (list python-attrs
                             python-immutables
                             python-prompt-toolkit
                             python-pygments
                             python-pyrsistent
                             python-pytest
                             python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://basilisp.readthedocs.io/en/latest/")
    (synopsis "A Clojure-like lisp written for Python")
    (description
     "This package provides a Clojure-like lisp written for Python.")
    (license license:expat)))
