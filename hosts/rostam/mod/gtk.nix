{ config, pkgs, lib, ... }:

{
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };

  home-manager.users.eudoxia.gtk = {
    enable = true;
    theme = {
      name = "Arc";
      package = pkgs.arc-theme;
    };
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-icon-theme;
    };
  };
}
