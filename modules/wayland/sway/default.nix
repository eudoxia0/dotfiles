{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/sway/config - - - - ${dotfilesDir}/modules/wayland/sway/sway.conf"
    "L+ /home/eudoxia/.config/swaylock/config - - - - ${dotfilesDir}/modules/wayland/sway/swaylock.conf"
  ];
}
