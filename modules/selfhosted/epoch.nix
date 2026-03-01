{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    systemd.user.services.epoch = {
      Unit = {
        Description = "epoch";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "/home/eudoxia/.eudoxia.d/bin/epoch serve";
        Restart = "on-failure";
        RestartSec = "5s";
        Environment = [
          "RUST_LOG=info"
          "EPOCH_PORT=12003"
          "EPOCH_DB_PATH=/home/eudoxia/root/Databases/epoch/epoch.db"
        ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
