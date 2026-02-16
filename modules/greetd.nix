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
        path = "${../wallpaper/panther.jpg}";
        fit = "Cover";
      };
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true;
}
