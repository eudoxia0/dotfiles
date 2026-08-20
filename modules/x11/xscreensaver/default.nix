{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    xscreensaver # needed for cli commands
  ];

  services.xscreensaver.enable = true;

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.xscreensaver - - - - ${dotfilesDir}/modules/x11/xscreensaver/xscreensaver.txt"
  ];
}
