{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    waybar
  ];

  home-manager.users.eudoxia.home.file.".config/waybar/config.jsonc".source = ./waybar.jsonc;
  home-manager.users.eudoxia.home.file.".config/waybar/style.css".source = ./style.css;
}
