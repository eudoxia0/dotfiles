{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.displayManager.ly.enable = true;
  services.displayManager.ly.settings = {
    animation = "matrix";
    bigclock = "none";
    brightness_down_key = "null";
    brightness_up_key = "null";
    clear_password = true;
    default_input = "password";
    hide_version_string = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/restart-ly".source = ./restart-ly.sh;
  };
}
