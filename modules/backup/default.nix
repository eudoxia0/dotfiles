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
    ".eudoxia.d/bin/e-backup".source = ./backup.sh;
  };
}
