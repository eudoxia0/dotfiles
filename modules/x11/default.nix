{
  config,
  pkgs,
  lib,
  dotfilesDir,
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
    arandr
    dmenu
    kitty
    rofi
    scrot
    xsecurelock
  ];
}
