{ config, pkgs, ... }:

{
    services.nix-daemon.enable = true;

    nix.extraOptions = ''
        auto-optimise-store = true
        experimental-features = nix-command flakes
        extra-platforms = x86_64-darwin aarch64-darwin
    '';

    nixpkgs.hostPlatform = "aarch64-darwin";

    programs.zsh.enable = true;

    users.users.eudoxia = {
        home = "/Users/eudoxia";
    };

    system.defaults = {
        dock = {
            autohide = false;
            orientation = "bottom";
        };
    };

    homebrew = {
        enable = true;

        # casks = [
        #     "emacs"
        # ];
    };
}
