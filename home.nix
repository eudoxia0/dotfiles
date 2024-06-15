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
        ];
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
        };
        home-manager = {
            enable = true;
        };
    };
}
