{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    xfce.thunar
    ffmpegthumbnailer # video thumbnails
    libgsf # odf thumbnails
    poppler # pdf thumbnails
    libraw # RAW thumbnails
    webp-pixbuf-loader # webp thumbnail
    xfce.tumbler # xfce thumbnail service
  ];
}
