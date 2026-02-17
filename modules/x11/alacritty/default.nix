{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    alacritty
  ];

  home-manager.users.eudoxia.home.file.".config/alacritty/alacritty.toml".source = ./config.toml;
}
