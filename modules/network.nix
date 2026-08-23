{
  config,
  pkgs,
  lib,
  ...
}:

{
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  services.mullvad-vpn.enable = true;

  # Enable the network manager applet (systray icon).
  programs.nm-applet.enable = true;

  # Need this to set the icons for the network manager applet.
  users.users.eudoxia.packages = [ pkgs.networkmanagerapplet ];
}
