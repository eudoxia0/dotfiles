{
  config,
  pkgs,
  lib,
  ...
}:

{
  systemd.user.services.metauro = {
    description = "metauro";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/metauro serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "METAURO_PORT=12005"
        "METAURO_DATA_DIRECTORY=/home/eudoxia/root/6-databases/metauro"
      ];
    };
  };
}
