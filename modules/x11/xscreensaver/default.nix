{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = with pkgs; [
    xscreensaver # needed for cli commands
  ];

  services.xscreensaver.enable = true;

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.xscreensaver - - - - ${dotfilesDir}/modules/x11/xscreensaver/xscreensaver.txt"
  ];
}
