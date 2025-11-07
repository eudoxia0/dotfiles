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
    options = "compose:ralt";
  };
  home-manager.users.eudoxia.home.packages = with pkgs; [
    scrot
    arandr
    xorg.xev
  ];
  home-manager.users.eudoxia.home.file = {
    ".XCompose".source = ./x11/XCompose;
  };
}
