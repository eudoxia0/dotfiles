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
        fit = "Contain";
      };
    };
  };

  security.pam.services.greetd.enableGnomeKeyring = true;
}
