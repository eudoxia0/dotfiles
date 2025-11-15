{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Other packages needed by Thunar.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    newsboat
  ];
}
