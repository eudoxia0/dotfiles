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

  home-manager.users.eudoxia.services = {
    network-manager-applet.enable = true;
  };

  # If nm-applet is not instaled explicitly it won't have icons.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    networkmanagerapplet
  ];
}
