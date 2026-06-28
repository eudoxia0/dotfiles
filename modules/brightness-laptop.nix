{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install brightnessctl.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    brightnessctl
  ];
}
