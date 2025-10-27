TARGETS := /Users/eudoxia/.aerospace.toml \
		/Users/eudoxia/.zshrc \
		/Users/eudoxia/.gitconfig \
		/Users/eudoxia/.global-gitignore \
		/Users/eudoxia/.config/espanso/config/default.yml \
		/Users/eudoxia/.config/espanso/match/base.yml \
		/Users/eudoxia/.config/ghostty/config \
		/Users/eudoxia/.config/zed/settings.json \
		/Users/eudoxia/.emacs.d/init.el \
		/Users/eudoxia/.local/bin/backup.sh \
		/Users/eudoxia/.local/bin/canonicalize-screenshots.py \
		/Users/eudoxia/.local/bin/decrypt.sh \
		/Users/eudoxia/.local/bin/disktemp.py \
		/Users/eudoxia/.local/bin/encrypt.sh \
		/Users/eudoxia/.local/bin/probe-port.sh \
		/Users/eudoxia/.local/bin/timestamp.py
.PHONY: all clean

all: $(TARGETS)

/Users/eudoxia/.aerospace.toml: config/aerospace.toml
	ln -sfn $(PWD)/config/aerospace.toml /Users/eudoxia/.aerospace.toml

/Users/eudoxia/.zshrc: config/zshrc.sh
	ln -sfn $(PWD)/config/zshrc.sh /Users/eudoxia/.zshrc

/Users/eudoxia/.gitconfig: config/gitconfig.txt
	ln -sfn $(PWD)/config/gitconfig.txt /Users/eudoxia/.gitconfig

/Users/eudoxia/.global-gitignore: config/global-gitignore.txt
	ln -sfn $(PWD)/config/global-gitignore.txt /Users/eudoxia/.global-gitignore

/Users/eudoxia/.config/espanso/config/default.yml:
	mkdir -p /Users/eudoxia/.config/espanso/config
	touch /Users/eudoxia/.config/espanso/config/default.yml

/Users/eudoxia/.config/espanso/match/base.yml: ../common/espanso.yaml
	mkdir -p /Users/eudoxia/.config/espanso/match
	ln -sfn /Users/eudoxia/dotfiles/common/espanso.yaml /Users/eudoxia/.config/espanso/match/base.yml

/Users/eudoxia/.config/ghostty/config: config/ghostty.txt
	mkdir -p /Users/eudoxia/.config/ghostty
	ln -sfn $(PWD)/config/ghostty.txt /Users/eudoxia/.config/ghostty/config

/Users/eudoxia/.config/zed/settings.json: config/zed.json
	mkdir -p /Users/eudoxia/.config/zed
	ln -sfn $(PWD)/config/zed.json /Users/eudoxia/.config/zed/settings.json

/Users/eudoxia/.emacs.d/init.el: config/init.el
	mkdir -p /Users/eudoxia/.emacs.d
	ln -sfn $(PWD)/config/init.el /Users/eudoxia/.emacs.d/init.el

/Users/eudoxia/.local/bin/backup.sh: config/bin/backup.sh
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/backup.sh /Users/eudoxia/.local/bin/backup.sh

/Users/eudoxia/.local/bin/canonicalize-screenshots.py: config/bin/canonicalize-screenshots.py
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/canonicalize-screenshots.py /Users/eudoxia/.local/bin/canonicalize-screenshots.py

/Users/eudoxia/.local/bin/decrypt.sh: config/bin/decrypt.sh
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/decrypt.sh /Users/eudoxia/.local/bin/decrypt.sh

/Users/eudoxia/.local/bin/disktemp.py: config/bin/disktemp.py
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/disktemp.py /Users/eudoxia/.local/bin/disktemp.py

/Users/eudoxia/.local/bin/encrypt.sh: config/bin/encrypt.sh
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/encrypt.sh /Users/eudoxia/.local/bin/encrypt.sh

/Users/eudoxia/.local/bin/probe-port.sh: config/bin/probe-port.sh
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/probe-port.sh /Users/eudoxia/.local/bin/probe-port.sh

/Users/eudoxia/.local/bin/timestamp.py: config/bin/timestamp.py
	mkdir -p /Users/eudoxia/.local/bin
	ln -sfn $(PWD)/config/bin/timestamp.py /Users/eudoxia/.local/bin/timestamp.py

clean:
	rm -f $(TARGETS)
