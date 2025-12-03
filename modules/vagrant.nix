{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    vagrant
  ];

  virtualisation.virtualbox.host.enable = true;
  users.users.eudoxia.extraGroups = [ "vboxusers" ];
}
