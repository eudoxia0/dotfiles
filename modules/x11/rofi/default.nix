{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    rofi
  ];
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/rofi-launcher.py" = {
      source = ./rofi-launcher.py;
      executable = true;
    };
  };
}
