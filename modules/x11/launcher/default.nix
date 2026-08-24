{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/bin/launcher - - - - ${dotfilesDir}/modules/x11/launcher/launcher.py"
  ];
}
