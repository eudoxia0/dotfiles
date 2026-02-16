{
  config,
  pkgs,
  lib,
  ...
}:

let
  swayGreetdConfig = pkgs.writeText "sway-greetd.conf" (builtins.readFile ./sway-greetd.conf);
in
{
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.sway}/bin/sway --config ${swayGreetdConfig}";
        user = "greeter";
      };
    };
  };

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
