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

update:
	emacs --script update-packages.el
