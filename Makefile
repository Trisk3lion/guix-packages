build-all:
	guix build -L . --quiet --keep-going \
		$$(guix package -A | grep "trisk/packages" | awk '{ print $1 "@" $2 }')

lint-all:
	guix lint -L . --no-network \
		$${guix package -A | grep "trisk/packages" | awk -F "[: \t]" '{ print $4 }'}

build-emacs-pgtk:
	guix build --load-path=. trisk-emacs-master-pgtk

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
	guix build --load-path=. rmapi

build-mermaid: trisk/packages/mermaid.scm
	guix build --load-path=. node-mermaid-js-mermaid-cli

build-guile-lsp: trisk/packages/mermaid.scm
	guix build --load-path=. guile-lsp-server

build-ciopfs: trisk/packages/ciopfs.scm
	guix build --load-path=. ciopfs

build-scrutiny-collector: trisk/packages/scrutiny.scm
	guix build --load-path=. scrutiny-collector

build-scrutiny-web: trisk/packages/scrutiny.scm
	guix build --load-path=. scrutiny-web

build-tailscale-vendored: trisk/packages/tailscale.scm
	guix build --load-path=. tailscale-vendored

build-squeezelite: trisk/packages/audio.scm
	guix build --load-path=. squeezelite

build-squeezelite-pulse: trisk/packages/audio.scm
	guix build --load-path=. squeezelite-pulse

build-docker-compose: trisk/packages/docker.scm
	guix build --load-path=. docker-compose-plugin

build-atuin: trisk/packages/atuin.scm
	guix build --load-path=. atuin

build-ghostty: trisk/packages/atuin.scm
	guix build --load-path=. ghostty

update:
	emacs --script update-packages.el
