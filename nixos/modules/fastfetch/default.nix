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

  home-manager.users.eudoxia.home.file.".local/share/eudoxia/nerv-color.txt".source =
    ./nerv/nerv-color.txt;
  home-manager.users.eudoxia.home.file.".local/bin/nervfetch".source = ./nerv/nervfetch.sh;
}
