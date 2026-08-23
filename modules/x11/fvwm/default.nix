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

  users.users.eudoxia.packages = [ pkgs.stalonetray ];

  home-manager.users.eudoxia = hm: {
    home = {
      file = {
        # Copy the fvwm config.
        ".fvwm/config".source = ./fvwm.txt;
        # Copy the CDE colors directory.
        ".fvwm/cde-colors" = {
          source = ./cde-colors;
          recursive = true;
        };
        # Copy the custom icons directory.
        ".fvwm/custom-icons" = {
          source = ./custom-icons;
          recursive = true;
        };
      };
    };
  };
}
