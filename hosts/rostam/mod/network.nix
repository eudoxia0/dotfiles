{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Network configuration
  networking.networkmanager.enable = true;
  networking.hostName = "rostam";

  # Enable the firewall
  networking.firewall.enable = true;
}
