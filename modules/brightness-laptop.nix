{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install brightnessctl.
  users.users.eudoxia.packages = [ pkgs.brightnessctl ];
}
