{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.dconf.enable = true;

  home-manager.users.eudoxia.gtk = {
    enable = true;
    theme = {
      name = "Adwaita";
      package = pkgs.gnome-themes-extra;
    };
    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };
  };

  # Install other themes so we can preview them.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    adwaita-icon-theme-legacy
    adwaita-icon-theme
  ];
}
