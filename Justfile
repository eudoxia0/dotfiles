PWD := `pwd`

install:
    brew bundle

apply:
    # Ensure directories exist
    mkdir -p ~/.config/ghostty
    mkdir -p ~/.config/zed
    mkdir -p ~/.emacs.d/eudoxia
    mkdir -p ~/.local/bin

    # Copy files
    cp "{{PWD}}/home/.config/ghostty/config" ~/.config/ghostty/config
    cp "{{PWD}}/home/.config/zed/settings.json" ~/.config/zed/settings.json
    cp "{{PWD}}/home/.emacs.d/init.el" ~/.emacs.d/init.el
    cp "{{PWD}}/home/.gitconfig" ~/.gitconfig
    cp "{{PWD}}/home/.global-gitignore" ~/.global-gitignore
    cp "{{PWD}}/home/.zshrc" ~/.zshrc

    # Copy directory contents
    cp -a "{{PWD}}/home/.emacs.d/eudoxia" ~/.emacs.d
    cp -a "{{PWD}}/home/.local/bin" ~/.local

clean:
    # Delete files
    rm -f ~/.config/ghostty/config
    rm -f ~/.config/zed/settings.json
    rm -f ~/.emacs.d/init.el
    rm -f ~/.gitconfig
    rm -f ~/.global-gitignore
    rm -f ~/.zshrc

    # Delete directories
    rm -rf ~/.emacs.d/eudoxia
    rm -rf ~/.local/bin
