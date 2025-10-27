ESPANSO_SRC   := common/espanso.yaml
ESPANSO_MACOS := macos/home/.config/espanso/match/base.yml

all: $(ESPANSO_MACOS)

$(ESPANSO_MACOS): $(ESPANSO_SRC)
	mkdir -p $(dir $@)
	ln -sf $(abspath $<) $@

clean:
	rm -f $(ESPANSO_MACOS)
