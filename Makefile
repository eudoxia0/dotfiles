DIR := home/.emacs.d/vendor
TARGETS := $(DIR)/hydra \
           $(DIR)/olivetti \
           $(DIR)/aircon-theme

default: all

all: $(TARGETS)

home/.emacs.d/vendor/hydra:
	git clone --depth=1 https://github.com/abo-abo/hydra.git $(DIR)/hydra
	rm -rf $(DIR)/hydra/.git

home/.emacs.d/vendor/olivetti:
	git clone --depth=1 https://github.com/rnkn/olivetti.git $(DIR)/olivetti
	rm -rf $(DIR)/olivetti/.git

home/.emacs.d/vendor/aircon-theme:
	git clone --depth=1 https://git.sr.ht/~chambln/aircon-theme.el $(DIR)/aircon-theme
	rm -rf $(DIR)/aircon-theme/.git

.PHONY: clean
clean:
	rm -rf $(TARGETS)
