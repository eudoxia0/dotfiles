{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };
  home-manager.users.eudoxia.home.packages = with pkgs; [
    scrot
    arandr
    xorg.xev
  ];
}
