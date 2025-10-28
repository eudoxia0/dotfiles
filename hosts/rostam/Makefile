# $< = first dep
# $@ = target
HOME    := /home/eudoxia
TARGETS := $(HOME)/.config/alacritty/alacritty.toml \
		$(HOME)/.config/espanso/match/base.yml \
		$(HOME)/.config/polybar/config.ini \
		$(HOME)/.config/sway/config \
		$(HOME)/.emacs.d/init.el \
		$(HOME)/.local/share/panther.jpg \
		$(HOME)/.stumpwmrc \
		$(HOME)/.xscreensaver

.PHONY: all recrank upgrade clean clean-nix

all: $(TARGETS)

$(HOME)/.config/alacritty/alacritty.toml: config/alacritty.toml
	mkdir -p $(HOME)/.config/alacritty
	cp $< $@

$(HOME)/.config/espanso/match/base.yml: ../../common/espanso.yaml
	mkdir -p $(HOME)/.config/espanso/match
	cp $< $@

$(HOME)/.config/polybar/config.ini: config/polybar.ini
	mkdir -p $(HOME)/.config/polybar
	cp $< $@

$(HOME)/.config/sway/config: config/sway.config
	mkdir -p $(HOME)/.config/sway
    cp $< $@

$(HOME)/.emacs.d/init.el: config/init.el
	mkdir -p $(HOME)/.emacs.d
	cp $< $@

$(HOME)/.local/share/panther.jpg: ../../common/panther.jpg
	mkdir -p $(HOME)/.local/share
	cp $< $@

$(HOME)/.stumpwmrc: config/stumpwmrc
	cp $< $@

$(HOME)/.xscreensaver: config/xscreensaver
	cp $< $@

clean:
	rm -f $(TARGETS)

recrank:
	sudo nixos-rebuild switch -I nixos-config=nix/rostam.nix

upgrade:
	sudo nix-channel --update
	sudo nixos-rebuild switch -I nixos-config=nix/rostam.nix --upgrade

clean-nix:
	sudo nix-env --delete-generations old
	sudo nix-collect-garbage -d
