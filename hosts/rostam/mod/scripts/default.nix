{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".local/bin/restart-display-manager".source = ./restart-display-manager.sh;
  };
}
