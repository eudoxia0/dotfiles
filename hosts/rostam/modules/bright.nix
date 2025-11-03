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

  # Create the i2c group.
  users.groups.i2c = { };

  # Add eudoxia to the i2c group.
  users.users.eudoxia.extraGroups = [ "i2c" ];

  # Add udev rules to allow the i2c group to access i2c devices.
  services.udev.extraRules = ''
    KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
  '';
}
