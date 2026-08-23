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
  users.users.eudoxia.packages = with pkgs; [
    arandr
    dmenu
    kitty
    rofi
    scrot
    xsecurelock
  ];
}
