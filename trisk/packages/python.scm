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
       (method url-fetch)
       (uri (pypi-uri "basilisp" version))
       (sha256
        (base32 "19mdi18l28s7ylqrgb6w5s89q7gdsxzrb8js5asr9c8lrmqs4hnb"))))
    (properties '(("upstream-name" . "basilisp")))
    (build-system pyproject-build-system)
    (arguments `(#:tests? #f))
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
     "This package provides a Clojure-like lisp written for Python.
PYTHONPYCACHEPREFIX needs to be set to a folder outside the /gnu/store in order for $code{basilisp} to byte compile it's namespaces;
@code{export PYTHONPYCACHEPREFIX=/path/to/project/.pycache}")
    (license license:expat)))
