{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  # Load uinput kernel module and set permissions for espanso
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules = ''
    KERNEL=="uinput", GROUP="input", MODE="0660"
  '';

  # Enable the Espanso service.
  services.espanso.enable = true;

  # Copy the config.
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/espanso/match/base.yml - - - - ${dotfilesDir}/modules/x11/espanso/espanso.yaml"
  ];
}
