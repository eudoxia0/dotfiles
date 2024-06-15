{ config, pkgs, ... }:

{
    services.nix-daemon.enable = true;
    users.users.eudoxia = {
      home = "/Users/eudoxia";
    };
}
