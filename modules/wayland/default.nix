{
  config,
  pkgs,
  lib,
  ...
}:

{
  users.users.eudoxia.packages = with pkgs; [
    mako
    slurp
    grim
    rofimoji
    wl-clipboard
    wlsunset
    wdisplays
    swaylock
    sway-contrib.grimshot
  ];
}
