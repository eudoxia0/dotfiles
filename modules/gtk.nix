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
    };
    iconTheme = {
      name = "elementary";
      package = pkgs.pantheon.elementary-icon-theme;
    };
  };

  # Install other themes so we can preview them.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    pantheon.elementary-gtk-theme
    pantheon.elementary-icon-theme
    adwaita-icon-theme-legacy
    arc-theme
  ];
}
