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
    dmenu
    kitty
    xsecurelock
    feh
  ];
}
