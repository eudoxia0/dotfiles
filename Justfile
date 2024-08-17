PWD := `pwd`

install:
    brew bundle

apply:
    ln -sf "{{PWD}}/home/.zprofile" ~/.zprofile
    ln -sf "{{PWD}}/home/.zshrc" ~/.zshrc

clean:
    rm -f ~/.zprofile
    rm -f ~/.zshrc
