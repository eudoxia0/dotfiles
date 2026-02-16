{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = [ pkgs.emote ];

  home-manager.users.eudoxia.systemd.user.services.emote = {
    Unit = {
      Description = "Emote emoji picker";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.emote}/bin/emote";
      Restart = "on-failure";
      RestartSec = "5s";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
