{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Copy the wallpapers directory.
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper" = {
    source = ./images;
    recursive = true;
  };

  # Install feh.
  home-manager.users.eudoxia.home.packages = [
    pkgs.feh
  ];
}
