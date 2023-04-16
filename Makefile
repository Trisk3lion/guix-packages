build-emacs:
	guix build --load-path=trisk/packages emacs-master

build-babashka:
	guix build --load-path=trisk/packages babashka
