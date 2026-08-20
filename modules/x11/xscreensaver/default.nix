{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    xscreensaver # needed for cli commands
  ];

  services.xscreensaver.enable = true;

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.xscreensaver - - - - /home/eudoxia/root/1-workspace/dotfiles/modules/x11/xscreensaver/xscreensaver.txt"
  ];
}
