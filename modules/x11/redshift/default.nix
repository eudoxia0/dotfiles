{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  services.redshift.enable = true;
  services.redshift.temperature = {
    day = 6500;
    night = 2500;
  };
  location = {
    latitude = -33.8;
    longitude = 151.2;
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/bin/start-redshift - - - - ${dotfilesDir}/modules/x11/redshift/start-redshift.sh"
    "L+ /home/eudoxia/.eudoxia.d/bin/stop-redshift - - - - ${dotfilesDir}/modules/x11/redshift/stop-redshift.sh"
  ];
}
