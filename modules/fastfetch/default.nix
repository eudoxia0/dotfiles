{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    fastfetch
  ];

  home-manager.users.eudoxia.home.file.".eudoxia.d/data/nerv/nerv-color.txt".source =
    ./nerv/nerv-color.txt;
  home-manager.users.eudoxia.home.file.".eudoxia.d/bin/nervfetch".source = ./nerv/nervfetch.sh;
}
