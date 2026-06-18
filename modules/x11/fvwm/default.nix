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
        # Symlink the fvwm config.
        ".fvwm/config".source =
          hm.config.lib.file.mkOutOfStoreSymlink "${hm.config.home.homeDirectory}/root/1-workspace/dotfiles/modules/x11/fvwm/fvwm.txt";
        # Copy the CDE colors directory.
        ".fvwm/CdeColors" = {
          source = ./cde;
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
