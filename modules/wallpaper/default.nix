{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper/panther.jpg".source = ./panther.jpg;
}
