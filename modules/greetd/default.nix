{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.regreet = {
    enable = true;
    settings = {
      background = {
        path = "/home/eudoxia/.eudoxia.d/data/wallpaper/panther.jpg";
        fit = "Cover";
      };
      GTK = {
        application_prefer_dark_theme = true;
      };
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true;
}
