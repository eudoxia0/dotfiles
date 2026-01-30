{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.sddm.enable = true;

  security.pam.services.sddm.enableGnomeKeyring = true;
}
