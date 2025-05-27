PWD := `pwd`

apply:
    mkdir -p ~/.config/ghostty
    mkdir -p ~/.config/zed
    mkdir -p ~/.emacs.d
    mkdir -p ~/.local/bin

    cp "{{PWD}}/home/.config/ghostty/config" ~/.config/ghostty/config
    cp "{{PWD}}/home/.config/zed/settings.json" ~/.config/zed/settings.json
    cp "{{PWD}}/home/.emacs.d/init.el" ~/.emacs.d/init.el
    cp "{{PWD}}/home/.gitconfig" ~/.gitconfig
    cp "{{PWD}}/home/.global-gitignore" ~/.global-gitignore
    cp "{{PWD}}/home/.zshrc" ~/.zshrc
    cp "{{PWD}}/home/.local/bin/backup.sh" ~/.local/bin/backup.sh
    cp "{{PWD}}/home/.local/bin/canonicalize-screenshots.py" ~/.local/bin/canonicalize-screenshots.py
    cp "{{PWD}}/home/.local/bin/decrypt.sh" ~/.local/bin/decrypt.sh
    cp "{{PWD}}/home/.local/bin/encrypt.sh" ~/.local/bin/encrypt.sh
    cp "{{PWD}}/home/.local/bin/probe-port.sh" ~/.local/bin/probe-port.sh
    cp "{{PWD}}/home/.local/bin/timestamp.py" ~/.local/bin/timestamp.py

clean:
    rm -f ~/.config/ghostty/config
    rm -f ~/.config/zed/settings.json
    rm -f ~/.emacs.d/init.el
    rm -f ~/.gitconfig
    rm -f ~/.global-gitignore
    rm -f ~/.zshrc
    rm -f ~/.local/bin/backup.sh
    rm -f ~/.local/bin/canonicalize-screenshots.py
    rm -f ~/.local/bin/decrypt.sh
    rm -f ~/.local/bin/encrypt.sh
    rm -f ~/.local/bin/probe-port.sh
    rm -f ~/.local/bin/timestamp.py
