{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  # Copy the wallpapers directory.
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/data/wallpaper - - - - ${dotfilesDir}/modules/wallpaper/images"
  ];

  # Install feh.
  users.users.eudoxia.packages = [
    pkgs.feh
  ];
}
