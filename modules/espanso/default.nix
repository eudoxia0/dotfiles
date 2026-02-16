{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Espanso on Wayland requires the input group for EVDEV access
  users.users.eudoxia.extraGroups = [ "input" ];

  # Load uinput kernel module and set permissions for espanso
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules = ''
    KERNEL=="uinput", GROUP="input", MODE="0660"
  '';

  home-manager.users.eudoxia.services.espanso.enable = true;

  # kdotool is required for text injection on Wayland
  home-manager.users.eudoxia.home.packages = with pkgs; [
    kdotool
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/espanso/match/base.yml".source = ./espanso.yaml;
  };
}
