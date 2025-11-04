{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Network configuration
  networking.networkmanager.enable = true;

  # Enable the firewall
  networking.firewall.enable = true;
}
