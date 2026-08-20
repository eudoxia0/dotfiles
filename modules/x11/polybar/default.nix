{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    (polybar.override {
      pulseSupport = true;
    })
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/polybar/config.ini - - - - ${dotfilesDir}/modules/x11/polybar/polybar.ini"
    "L+ /home/eudoxia/.config/polybar/world-clock.sh - - - - ${dotfilesDir}/modules/x11/polybar/world-clock.sh"
  ];
}
