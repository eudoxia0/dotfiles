{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xscreensaver.enable = true;

  home-manager.users.eudoxia.home.file = {
    ".xscreensaver".source = ./xscreensaver.txt;
  };
}
