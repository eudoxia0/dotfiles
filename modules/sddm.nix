{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  security.pam.services.sddm.enableGnomeKeyring = true;
}
