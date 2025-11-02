{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    ddcutil
  ];

  # Add i2c to control monitor brightness from the terminal. Needed by
  # ddcutil.
  boot.kernelModules = [ "i2c-dev" ];
}
