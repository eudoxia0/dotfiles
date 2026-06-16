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

  # Copy the CDE colors directory.
  home-manager.users.eudoxia.home.file.".fvwm/CdeColors" = {
    source = ./cde;
    recursive = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".fvwm/config".source = ./fvwm.txt;
  };

  home-manager.users.eudoxia.home.packages = with pkgs; [
    stalonetray
  ];
}
