{ pkgs, ... }:

{
    home = {
        username = "eudoxia";
        homeDirectory = "/Users/eudoxia";
        stateVersion = "24.05";

        packages = with pkgs; [
            coreutils
            neofetch
            curl
            python3
        ];

        file = {
            ".gitconfig".source = ./home/.gitconfig;
            ".gitignore".source = ./home/.global-gitignore;
            ".config/zed/settings.json".source = ./home/.config/zed/settings.json;
            ".emacs.d/init.el".source = ./home/.emacs.d/init.el;
            ".hushlogin".text = "";
            ".local/bin/" = {
                source = ./home/.local/bin;
                recursive = true;
            };
        };
    };

    programs = {
        zsh = {
            enable = true;

            shellAliases = {
                ls   = "ls -1 --color";
                gs   = "git status";
                gits = "git status";
                gb   = "git branch";
                gco  = "git checkout";
                gcam = "git commit -a -m";
                gu   = "git push -u origin HEAD";
                gd   = "git pull origin";
                cf   = "cargo fmt";
                ck   = "cargo check";
            };
            sessionVariables = {
              PATH = "$HOME/.local/bin:$PATH";
            };
        };
        home-manager = {
            enable = true;
        };
    };
}
