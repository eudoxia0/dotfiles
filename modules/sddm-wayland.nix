{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.wayland.compositor = "kwin";
  systemd.services.display-manager.environment = {
    KWIN_FORCE_SW_CURSOR = "1";
  }
}
