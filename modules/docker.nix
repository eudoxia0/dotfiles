{
  config,
  pkgs,
  lib,
  ...
}:

{
  virtualisation.docker = {
    enable = true;
  };

  users.users.eudoxia.extraGroups = [ "docker" ];
}
