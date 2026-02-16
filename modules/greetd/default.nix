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
        fit = "Fill";
      };
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true;
}
