{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    (polybar.override {
      pulseSupport = true;
    })
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/polybar/config.ini".source = ./polybar.ini;
  };
}
