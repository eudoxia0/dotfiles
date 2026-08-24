{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/mimeapps.list - - - - ${dotfilesDir}/modules/mimeapps/mimeapps.list"
  ];
}
