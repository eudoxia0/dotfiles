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

  # Make Electron apps use Wayland.
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
