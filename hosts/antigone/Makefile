# $< = first dep
# $@ = target
HOME    := /Users/eudoxia
TARGETS := $(HOME)/.aerospace.toml \
		$(HOME)/.config/espanso/config/default.yml \
		$(HOME)/.config/espanso/match/base.yml \
		$(HOME)/.config/ghostty/config \
		$(HOME)/.config/zed/settings.json \
		$(HOME)/.emacs.d/init.el \
		$(HOME)/.gitconfig \
		$(HOME)/.global-gitignore \
		$(HOME)/.local/bin/backup.sh \
		$(HOME)/.local/bin/canonicalize-screenshots.py \
		$(HOME)/.local/bin/decrypt.sh \
		$(HOME)/.local/bin/disktemp.py \
		$(HOME)/.local/bin/encrypt.sh \
		$(HOME)/.local/bin/probe-port.sh \
		$(HOME)/.local/bin/timestamp.py
		$(HOME)/.zshrc \
.PHONY: all clean

all: $(TARGETS)

$(HOME)/.aerospace.toml: config/aerospace.toml
	cp $< $@

$(HOME)/.zshrc: config/zshrc.sh
	cp $< $@

$(HOME)/.gitconfig: config/gitconfig.txt
	cp $< $@

$(HOME)/.global-gitignore: config/global-gitignore.txt
	cp $< $@

$(HOME)/.config/espanso/config/default.yml:
	mkdir -p $(HOME)/.config/espanso/config
	touch $@

$(HOME)/.config/espanso/match/base.yml: ../../common/espanso.yaml
	mkdir -p $(HOME)/.config/espanso/match
	cp $< $@

$(HOME)/.config/ghostty/config: config/ghostty.txt
	mkdir -p $(HOME)/.config/ghostty
	cp $< $@

$(HOME)/.config/zed/settings.json: config/zed.json
	mkdir -p $(HOME)/.config/zed
	cp $< $@

$(HOME)/.emacs.d/init.el: config/init.el
	mkdir -p $(HOME)/.emacs.d
	cp $< $@

$(HOME)/.local/bin/%: config/bin/%
	mkdir -p $(HOME)/.local/bin
	cp $< $@

clean:
	rm -f $(TARGETS)
