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
      name = "Clearlooks-Phenix";
      package = pkgs.clearlooks-phenix;
    };
    iconTheme = {
      name = "Bluecurve";
    };
  };

  # Install other themes so we can preview them.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    adwaita-icon-theme-legacy
    adwaita-icon-theme
  ];
}
