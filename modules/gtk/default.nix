{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

let
  bluecurve = pkgs.callPackage ./bluecurve.nix { };
in
{
  programs.dconf.enable = true;

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/gtk-3.0/settings.ini - - - - ${dotfilesDir}/modules/gtk/gtk3-settings.ini"
    "L+ /home/eudoxia/.config/gtk-4.0/settings.ini - - - - ${dotfilesDir}/modules/gtk/gtk4-settings.ini"
  ];

  # Install other themes so we can preview them.
  users.users.eudoxia.packages = with pkgs; [
    adwaita-icon-theme-legacy
    adwaita-icon-theme
    bluecurve
  ];

  # On X11, new GTK4 windows appear as a black rectangle for a few frames
  # before being painted. Using the software renderer fixes this at the cost
  # of losing GPU acceleration.
  environment.sessionVariables = {
    GSK_RENDERER = "cairo";
  };
}
