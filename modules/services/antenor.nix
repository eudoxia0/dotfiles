{
  config,
  pkgs,
  lib,
  ...
}:

{
  systemd.user.services.antenor = {
    description = "antenor";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/antenor serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "ANTENOR_PORT=12004"
        "ANTENOR_DATA_DIRECTORY=/home/eudoxia/root/6-databases/antenor"
      ];
    };
  };
}
