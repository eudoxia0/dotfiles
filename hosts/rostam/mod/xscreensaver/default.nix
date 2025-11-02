{ config, pkgs, lib, ... }:

{
  services.xscreensaver.enable = true;

  environment.systemPackages = with pkgs; [
    xscreensaver
  ];

  home-manager.users.eudoxia.home.file = {
    ".xscreensaver".source = ./xscreensaver.txt;
  };
}
