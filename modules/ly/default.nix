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
    custom_sessions = "/etc/xdg/custom-sessions/";
    default_input = "password";
    hide_version_string = true;
  };

  environment.etc."xdg/custom-sessions/tty.desktop".text = ''
    [Desktop Entry]
    Name=tty
    Exec=${pkgs.nushell}/bin/nu
    DesktopNames=null
    Terminal=true
  '';

  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/restart-ly".source = ./restart-ly.sh;
  };
}
