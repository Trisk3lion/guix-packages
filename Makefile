build-emacs:
	guix build --load-path=. emacs-master

build-babashka:
	guix build --load-path=. babashka

build-clj-kondo:
	guix build --load-path=. clj-kondo
