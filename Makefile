build-emacs-pgtk:
	guix build --load-path=. trisk-emacs-pgtk

build-emacs-master:
	guix build --load-path=. trisk-emacs-master

build-babashka:
	guix build --load-path=. babashka

build-clj-kondo:
	guix build --load-path=. clj-kondo

build-tailscale:
	guix build --load-path=. tailscale

build-datalevin:
	guix build --load-path=. datalevin

build-protojure:
	guix build --load-path=. protoc-gen-clojure

build-clojure-lsp:
	guix build --load-path=. clojure-lsp

build-hunspell-sv:
	guix build --load-path=. hunspell-dict-sv

build-gitstatus:
	guix build --load-path=. gitstatus

build-lsp-booster:
	guix build --load-path=. emacs-lsp-booster

build-mu:
	guix build --load-path=. mu

update:
	emacs --script update-packages.el
