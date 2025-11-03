{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install ddcutil, a cli program to control the monitor's brightness.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    ddcutil
  ];

  # Add i2c to control monitor brightness from the terminal. Needed by
  # ddcutil.
  boot.kernelModules = [ "i2c-dev" ];

  # Add eudoxia to the i2c group.
  users.users.eudoxia.extraGroups = [ "i2c" ];
}
