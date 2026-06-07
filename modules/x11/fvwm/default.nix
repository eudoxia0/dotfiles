{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.fvwm3 = {
    enable = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".fvwm/config".source = ./fvwm.txt;
  };
}
