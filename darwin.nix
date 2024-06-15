{ config, pkgs, ... }:

{
    services.nix-daemon.enable = true;

    nix.extraOptions = ''
       auto-optimise-store = true
       experimental-features = nix-command flakes
       extra-platforms = aarch64-darwin
    '';

    environment.systemPackages = [
        pkgs.neofetch
    ];

    programs.zsh.enable = true;

    users.users.eudoxia = {
        home = "/Users/eudoxia";
    };
}
