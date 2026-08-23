{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  services.xserver.windowManager.fvwm3 = {
    enable = true;
  };

  users.users.eudoxia.packages = [ pkgs.stalonetray ];

  systemd.tmpfiles.rules = [
    # Copy the FVWM config.
    "L+ /home/eudoxia/.fvwm/config - - - - ${dotfilesDir}/modules/x11/fvwm/fvwm.txt"
    # Copy the CDE colors directory.
    "L+ /home/eudoxia/.fvwm/cde-colors - - - - ${dotfilesDir}/modules/x11/fvwm/cde-colors"
    # Copy the custom icons directory.
    "L+ /home/eudoxia/.fvwm/custom-icons - - - - ${dotfilesDir}/modules/x11/fvwm/custom-icons"
  ];
}
