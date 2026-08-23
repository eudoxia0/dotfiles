{
  config,
  pkgs,
  lib,
  ...
}:

{
  users.users.eudoxia.packages = with pkgs; [
    v4l-utils
  ];
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];
}
