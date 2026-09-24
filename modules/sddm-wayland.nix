{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.sddm.wayland.enable = true;
}
