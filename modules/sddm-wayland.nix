{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.wayland.compositor = "kwin";
}
