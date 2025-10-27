# $< = first dep
# $@ = target
HOME    := /Users/eudoxia
TARGETS := $(HOME)/.aerospace.toml \
		$(HOME)/.zshrc \
		$(HOME)/.gitconfig \
		$(HOME)/.global-gitignore \
		$(HOME)/.config/espanso/config/default.yml \
		$(HOME)/.config/espanso/match/base.yml \
		$(HOME)/.config/ghostty/config \
		$(HOME)/.config/zed/settings.json \
		$(HOME)/.emacs.d/init.el \
		$(HOME)/.local/bin/backup.sh \
		$(HOME)/.local/bin/canonicalize-screenshots.py \
		$(HOME)/.local/bin/decrypt.sh \
		$(HOME)/.local/bin/disktemp.py \
		$(HOME)/.local/bin/encrypt.sh \
		$(HOME)/.local/bin/probe-port.sh \
		$(HOME)/.local/bin/timestamp.py
.PHONY: all clean

all: $(TARGETS)

$(HOME)/.aerospace.toml: config/aerospace.toml
	ln -sfn $(PWD)/config/aerospace.toml $(HOME)/.aerospace.toml

$(HOME)/.zshrc: config/zshrc.sh
	ln -sfn $(PWD)/config/zshrc.sh $(HOME)/.zshrc

$(HOME)/.gitconfig: config/gitconfig.txt
	ln -sfn $(PWD)/config/gitconfig.txt $(HOME)/.gitconfig

$(HOME)/.global-gitignore: config/global-gitignore.txt
	ln -sfn $(PWD)/config/global-gitignore.txt $(HOME)/.global-gitignore

$(HOME)/.config/espanso/config/default.yml:
	mkdir -p $(HOME)/.config/espanso/config
	touch $(HOME)/.config/espanso/config/default.yml

$(HOME)/.config/espanso/match/base.yml: ../common/espanso.yaml
	mkdir -p $(HOME)/.config/espanso/match
	ln -sfn $(HOME)/dotfiles/common/espanso.yaml $(HOME)/.config/espanso/match/base.yml

$(HOME)/.config/ghostty/config: config/ghostty.txt
	mkdir -p $(HOME)/.config/ghostty
	ln -sfn $(PWD)/config/ghostty.txt $(HOME)/.config/ghostty/config

$(HOME)/.config/zed/settings.json: config/zed.json
	mkdir -p $(HOME)/.config/zed
	ln -sfn $(PWD)/config/zed.json $(HOME)/.config/zed/settings.json

$(HOME)/.emacs.d/init.el: config/init.el
	mkdir -p $(HOME)/.emacs.d
	ln -sfn $(PWD)/config/init.el $(HOME)/.emacs.d/init.el

$(HOME)/.local/bin/backup.sh: config/bin/backup.sh
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/backup.sh $(HOME)/.local/bin/backup.sh

$(HOME)/.local/bin/canonicalize-screenshots.py: config/bin/canonicalize-screenshots.py
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/canonicalize-screenshots.py $(HOME)/.local/bin/canonicalize-screenshots.py

$(HOME)/.local/bin/decrypt.sh: config/bin/decrypt.sh
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/decrypt.sh $(HOME)/.local/bin/decrypt.sh

$(HOME)/.local/bin/disktemp.py: config/bin/disktemp.py
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/disktemp.py $(HOME)/.local/bin/disktemp.py

$(HOME)/.local/bin/encrypt.sh: config/bin/encrypt.sh
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/encrypt.sh $(HOME)/.local/bin/encrypt.sh

$(HOME)/.local/bin/probe-port.sh: config/bin/probe-port.sh
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/probe-port.sh $(HOME)/.local/bin/probe-port.sh

$(HOME)/.local/bin/timestamp.py: config/bin/timestamp.py
	mkdir -p $(HOME)/.local/bin
	ln -sfn $(PWD)/config/bin/timestamp.py $(HOME)/.local/bin/timestamp.py

clean:
	rm -f $(TARGETS)
