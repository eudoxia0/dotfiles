{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".config/swaylock/config".source = ./swaylock.conf;
  };
}
