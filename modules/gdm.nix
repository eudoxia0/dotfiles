{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.gdm.enable = true;
}
