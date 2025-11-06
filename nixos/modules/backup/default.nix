{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    rsync
  ];

  home-manager.users.eudoxia.home.file = {
    ".local/bin/backup".source = ./backup.sh;
  };
}
