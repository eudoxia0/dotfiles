{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.beets ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/beets/config.yaml - - - - ${dotfilesDir}/modules/beets/beets.yaml"
  ];
}
