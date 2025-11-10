{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
}
