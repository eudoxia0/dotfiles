{ pkgs, ... }:

{
    home = {
        username = "eudoxia";
        homeDirectory = "/Users/eudoxia";
        stateVersion = "24.05";
    };

    programs = {
        zsh = {
            enable = true;

            shellAliases = {
                ls = "ls --color";
            };
        };
        home-manager = {
            enable = true;
        };
    };
}
