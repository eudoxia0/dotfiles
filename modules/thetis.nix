{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    systemd.user.services.zetanom = {
      Unit = {
        Description = "thetis";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "/home/eudoxia/.eudoxia.d/bin/thetis serve";
        Restart = "on-failure";
        RestartSec = "5s";
        EnvironmentFile = "/home/eudoxia/Root/Databases/thetis/config/thetis.env"
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
