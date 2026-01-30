{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.niri.enable = true;

  home-manager.users.eudoxia.home.packages = with pkgs; [
    waybar
    fuzzel
    swaylock
  ];

  home-manager.users.eudoxia.home.file.".config/niri/config.kdl".source = ./niri.kdl;
}
