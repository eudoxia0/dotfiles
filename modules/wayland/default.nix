{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    mako
    slurp
    grim
    rofimoji
    wl-clipboard
    wlsunset
    swaylock
  ];
}
