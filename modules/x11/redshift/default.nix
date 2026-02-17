{
  config,
  pkgs,
  lib,
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

  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/start-redshift" = {
      text = ''
        #/bin/sh
        systemctl --user start redshift
      '';
      executable = true;
    };
    ".eudoxia.d/bin/stop-redshift" = {
      text = ''
        #/bin/sh
        systemctl --user stop redshift
      '';
      executable = true;
    };
  };
}
