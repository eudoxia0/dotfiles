{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Load uinput kernel module and set permissions for espanso
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules = ''
    KERNEL=="uinput", GROUP="input", MODE="0660"
  '';

  home-manager.users.eudoxia.services.espanso = {
    enable = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".config/espanso/match/base.yml".source = ./espanso.yaml;
  };
}
