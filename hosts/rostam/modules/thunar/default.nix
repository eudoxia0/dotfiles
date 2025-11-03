{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.thunar = {
    enable = true;
    plugins = [
      pkgs.xfce.thunar-archive-plugin
      pkgs.xfce.thunar-volman
    ];
  };

  # mount etc.
  services.gvfs.enable = true;

  # thumbnails service
  services.tumbler.enable = true;

  home-manager.users.eudoxia.home.packages = with pkgs; [
    bzip2
    ffmpegthumbnailer # video thumbnails
    gnutar
    gzip
    libgsf # odf thumbnails
    libraw # RAW thumbnails
    lz4
    mate.engrampa # open archive files
    poppler # pdf thumbnails
    unrar
    unzip
    webp-pixbuf-loader # webp thumbnail
    xclip # to implement the "copy path" custom action
    xfce.xfconf # query xfce config
    xz
    zip
    zstd
  ];

  # Configure custom Thunar actions.
  home-manager.users.eudoxia.home.file = {
    ".config/Thunar/uca.xml".source = ./actions.xml;
  };
}
