{
  config,
  pkgs,
  lib,
  ...
}:

{
  systemd.user.services.xcape = {
    description = "xcape";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      Type = "forking";
      ExecStart = "${lib.getExe pkgs.xcape} -e 'Shift_L=parenleft;Shift_R=parenright'";
    };
  };
}
