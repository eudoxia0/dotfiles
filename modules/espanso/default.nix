{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Espanso on Wayland requires the input group for EVDEV access
  users.users.eudoxia.extraGroups = [ "input" ];

  home-manager.users.eudoxia.services.espanso.enable = true;

  # kdotool is required for text injection on Wayland
  home-manager.users.eudoxia.home.packages = with pkgs; [
    kdotool
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/espanso/match/base.yml".source = ./espanso.yaml;
  };
}
