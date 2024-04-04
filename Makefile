DIR := home/.emacs.d/vendor
TARGETS := $(DIR)/hydra \
           $(DIR)/olivetti \
           $(DIR)/aircon-theme \
           $(DIR)/markdown-mode \
           $(DIR)/s

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

home/.emacs.d/vendor/markdown-mode:
	git clone --depth=1 https://github.com/jrblevin/markdown-mode.git $(DIR)/markdown-mode
	rm -rf $(DIR)/markdown-mode/.git

home/.emacs.d/vendor/s:
	git clone --depth=1 https://github.com/magnars/s.el.git $(DIR)/s
	rm -rf $(DIR)/s/.git

.PHONY: clean
clean:
	rm -rf $(TARGETS)
