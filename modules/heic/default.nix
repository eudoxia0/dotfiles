{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    libheif
  ];
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/e-heic2jpg".source = ./heic2jpg.sh;
  };
}
