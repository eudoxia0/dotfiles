{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    systemd.user.services.pm = {
      Unit = {
        Description = "pm";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "/home/eudoxia/.eudoxia.d/bin/pm";
        Restart = "on-failure";
        RestartSec = "5s";
        Environment = [
          "RUST_LOG=info"
          "PM_PORT=12006"
          "PM_DB_PATH=/home/eudoxia/root/6-databases/pm/pm.sqlite3"
        ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
