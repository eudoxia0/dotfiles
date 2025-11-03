{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    glib
    ffmpegthumbnailer
    poppler
    libraw
    sushi
    webp-pixbuf-loader
    xfce.tumbler
  ];
}
