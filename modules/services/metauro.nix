{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    systemd.user.services.metauro = {
      Unit = {
        Description = "metauro";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "/home/eudoxia/.eudoxia.d/bin/metauro serve";
        Restart = "on-failure";
        RestartSec = "5s";
        Environment = [
          "RUST_LOG=info"
          "METAURO_PORT=12005"
          "METAURO_DATA_DIRECTORY=/home/eudoxia/root/6-databases/metauro"
        ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
