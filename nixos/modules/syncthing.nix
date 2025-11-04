{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.services.syncthing = {
    enable = true;
    guiAddress = "127.0.0.1:8384";
  };
}
