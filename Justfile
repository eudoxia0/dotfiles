PWD := `pwd`

install:
    brew bundle

apply:
    # Link files
    ln -sf "{{PWD}}/home/.config/kitty/kitty.conf" ~/.config/kitty/kitty.conf
    ln -sf "{{PWD}}/home/.config/zed/settings.json" ~/.config/zed/settings.json
    ln -sf "{{PWD}}/home/.emacs.d/init.el" ~/.emacs.d/init.el
    ln -sf "{{PWD}}/home/.gitconfig" ~/.gitconfig
    ln -sf "{{PWD}}/home/.global-gitignore" ~/.global-gitignore
    ln -sf "{{PWD}}/home/.zprofile" ~/.zprofile
    ln -sf "{{PWD}}/home/.zshrc" ~/.zshrc
    # Link directories
    ln -sf "{{PWD}}/home/.emacs.d/eudoxia/" ~/.emacs.d/eudoxia
    ln -sf "{{PWD}}/home/.local/bin/" ~/.local/bin

clean:
    # Unlink files
    rm -f ~/.config/zed/settings.json
    rm -f ~/.emacs.d/init.el
    rm -f ~/.gitconfig
    rm -f ~/.global-gitignore
    rm -f ~/.zprofile
    rm -f ~/.zshrc
    # Unlink directories
    rm -f ~/.emacs.d/eudoxia
    rm -f ~/.local/bin
