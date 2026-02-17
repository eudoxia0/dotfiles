{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.stumpwm = {
    enable = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".stumpwm.d/init.lisp".source = ./init.lisp;
    ".stumpwm.d/gaps.lisp".source = ./gaps.lisp;
    ".stumpwm.d/bright.lisp".source = ./bright.lisp;
  };
}
