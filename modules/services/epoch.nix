{
  config,
  pkgs,
  lib,
  ...
}:

{
  systemd.user.services.epoch = {
    description = "epoch";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/epoch serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "EPOCH_PORT=12003"
        "EPOCH_DB_PATH=/home/eudoxia/root/6-databases/epoch/epoch.db"
      ];
    };
  };
}
