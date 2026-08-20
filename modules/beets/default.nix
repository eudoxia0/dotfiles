{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    beets
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/beets/config.yaml - - - - ${dotfilesDir}/modules/beets/beets.yaml"
  ];
}
