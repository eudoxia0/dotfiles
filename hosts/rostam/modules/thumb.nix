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
    gnome.sushi
    webp-pixbuf-loader
    xfce.tumbler
  ];
}
