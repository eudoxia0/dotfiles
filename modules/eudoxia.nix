{
  config,
  pkgs,
  lib,
  ...
}:

{
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  # Disable automatic creation of XDG user directories.
  home-manager.users.eudoxia.xdg.userDirs = {
    enable = true;
    createDirectories = false;
  };
}
