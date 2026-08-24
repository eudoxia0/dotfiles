{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.syncthing = {
    enable = true;
    guiAddress = "127.0.0.1:8384";
    user = "eudoxia";
    group = "users";
    configDir = "/home/eudoxia/.config/syncthing";
    openDefaultPorts = true;
  };
}
