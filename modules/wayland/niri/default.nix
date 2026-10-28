{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  programs.niri.enable = true;

  users.users.eudoxia.packages = with pkgs; [
    swaybg
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/niri/config.kdl - - - - ${dotfilesDir}/modules/wayland/niri/niri.kdl"
  ];
}
