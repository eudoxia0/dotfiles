{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  services.xserver.windowManager.bspwm = {
    enable = true;
    sxhkd.package = pkgs.sxhkd;
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/bspwm/bspwmrc - - - - ${dotfilesDir}/modules/x11/bspwm/bspwmrc"
    "L+ /home/eudoxia/.config/sxhkd/sxhkdrc - - - - ${dotfilesDir}/modules/x11/bspwm/sxhkdrc"
  ];
}
