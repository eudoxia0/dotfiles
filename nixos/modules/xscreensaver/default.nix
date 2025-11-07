{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xscreensaver.enable = true;

  environment.systemPackages = with pkgs; [
    xscreensaver
  ];

  systemd.user.services.xscreensaver = {
    Service = {
      Restart = "always";
    };
  };

  home-manager.users.eudoxia.home.file = {
    ".xscreensaver".source = ./xscreensaver.txt;
  };
}
