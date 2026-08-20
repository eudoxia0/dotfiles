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

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.stumpwm.d/init.lisp - - - - /home/eudoxia/root/1-workspace/dotfiles/modules/x11/stumpwm/init.lisp"
    "L+ /home/eudoxia/.stumpwm.d/gaps.lisp - - - - /home/eudoxia/root/1-workspace/dotfiles/modules/x11/stumpwm/gaps.lisp"
  ];
}
