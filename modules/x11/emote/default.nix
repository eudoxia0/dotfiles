{
  config,
  pkgs,
  lib,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.emote ];

  systemd.user.services.emote = {
    description = "Emote emoji picker";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = lib.getExe pkgs.emote;
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
