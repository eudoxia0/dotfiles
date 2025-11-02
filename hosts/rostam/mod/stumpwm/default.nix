{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.stumpwm.enable = true;

  home-manager.users.eudoxia.home.packages = with pkgs; [
    feh
  ];

  home-manager.users.eudoxia.home.file = {
    ".stumpwm.d/init.lisp".source = ./stumpwm.lisp;
  };
}
