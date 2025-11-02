# $< = first dep
# $@ = target
HOME    := /home/eudoxia
TARGETS := $(HOME)/.config/polybar/config.ini \
		$(HOME)/.local/share/panther.jpg \
		$(HOME)/.stumpwm.d/init.lisp \
		$(HOME)/.local/bin/restart-display-manager \
		$(HOME)/.cagebreak \
		$(HOME)/.xscreensaver

.PHONY: all recrank upgrade clean clean-nix

all: $(TARGETS)

$(HOME)/.cagebreak: config/cagebreak.conf
	cp $< $@

$(HOME)/.config/polybar/config.ini: config/polybar.ini
	mkdir -p $(HOME)/.config/polybar
	cp $< $@

config/inform7.el:
	curl -sL -o $@ "https://raw.githubusercontent.com/alexispurslane/inform7-mode/refs/heads/master/inform7.el"

$(HOME)/.local/share/panther.jpg: panther.jpg
	mkdir -p $(HOME)/.local/share
	cp $< $@

$(HOME)/.stumpwm.d/init.lisp: config/stumpwmrc.lisp
	cp $< $@

$(HOME)/.xscreensaver: config/xscreensaver.txt
	cp $< $@

$(HOME)/.local/bin/restart-display-manager: scripts/restart-display-manager.sh
	mkdir -p $(HOME)/.local/bin
	cp $< $@

recrank:
	sudo nixos-rebuild switch --flake .#rostam

upgrade:
	nix flake update
	sudo nixos-rebuild switch --flake .#rostam

clean-nix:
	sudo nix-collect-garbage --delete-older-than 7d
