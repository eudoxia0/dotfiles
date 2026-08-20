{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  services.xserver.windowManager.stumpwm = {
    enable = true;
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.stumpwm.d/init.lisp - - - - ${dotfilesDir}/modules/x11/stumpwm/init.lisp"
    "L+ /home/eudoxia/.stumpwm.d/gaps.lisp - - - - ${dotfilesDir}/modules/x11/stumpwm/gaps.lisp"
  ];
}
