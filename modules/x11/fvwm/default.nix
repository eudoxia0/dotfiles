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

  home-manager.users.eudoxia = hm: {
    home = {
      packages = [ pkgs.stalonetray ];
      file = {
        # Copy the fvwm config.
        ".fvwm/config".source = ./fvwm.txt;
        # Copy the CDE colors directory.
        ".fvwm/CdeColors" = {
          source = ./cde;
          recursive = true;
        };
      };
    };
  };
}
