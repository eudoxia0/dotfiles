{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    xscreensaver # needed for cli commands
  ];

  services.xscreensaver.enable = true;

  home-manager.users.eudoxia.home.file = {
    ".xscreensaver".source = ./xscreensaver.txt;
  };
}
