{ config, pkgs, lib, ... }:

{
  services.xserver.windowManager.stumpwm.enable = true;

  environment.systemPackages = with pkgs; [
    (polybar.override {
      pulseSupport = true;
    })
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/polybar/config.ini".source = ./polybar.ini;
  };
}
