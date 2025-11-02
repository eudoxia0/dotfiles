{
  config,
  pkgs,
  lib,
  ...
}:

{
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
