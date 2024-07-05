build-emacs-pgtk:
	guix build --load-path=. trisk-emacs-pgtk

build-emacs-master:
	guix build --load-path=. trisk-emacs-master

build-emacs-master-minimal:
	guix build --load-path=. emacs-master-minimal

build-babashka:
	guix build --load-path=. babashka

build-clj-kondo:
	guix build --load-path=. clj-kondo

build-tailscale-amd64:
	guix build --load-path=. tailscale-amd64-bin

build-tailscale-arm:
	guix build --load-path=. tailscale-arm-bin

build-tailscale-arm64:
	guix build --load-path=. tailscale-arm64-bin

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

build-rdrview:
	guix build --load-path=. rdrview

build-syncthing-goamd64v3:
	guix build --load-path=. syncthing-amd64v3

build-syncthing-armhf:
	guix build --load-path=. syncthing-armhf

build-calibre:
	guix build --load-path=. calibre-latest

build-fastfetch:
	guix build --load-path=. fastfetch

build-cobol-lsp:
	guix build --load-path=. che-lsp-for-cobol

build-kohighlights: trisk/packages/ebook.scm
	guix build --load-path=. kohighlights

build-rcu: trisk/packages/ebook.scm
	guix build --load-path=. rcu

build-rmapi: trisk/packages/remarkable.scm
	guix build --load-path=. go-github-com-juruen-rmapi

build-mermaid: trisk/packages/mermaid.scm
	guix build --load-path=. node-mermaid-js-mermaid-cli

update:
	emacs --script update-packages.el
