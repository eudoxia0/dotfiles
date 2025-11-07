{
  config,
  pkgs,
  lib,
  ...
}:

{
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  programs.nm-applet.enable = true;
  services.mullvad-vpn.enable = true;
}
