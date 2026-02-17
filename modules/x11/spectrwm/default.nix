{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.spectrwm.enable = true;

  home-manager.users.eudoxia.xdg.configFile = {
    "spectrwm/spectrwm.conf".source = ./spectrwm.conf;
  };
}
