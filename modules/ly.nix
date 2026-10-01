{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.ly.enable = true;

  services.displayManager.ly.x11Support = false;

  services.displayManager.ly.settings = {
    animation = "matrix";
    clear_password = true;
    default_input = "password";
    hide_version_string = true;
  };

  security.pam.services.gdm.enableGnomeKeyring = true;
}
