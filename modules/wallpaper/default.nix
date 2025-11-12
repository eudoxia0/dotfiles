{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper/panther.jpg".source = ./panther.jpg;
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper/semiramis.jpg".source =
    ./semiramis.jpg;
}
