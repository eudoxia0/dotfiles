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
    ffmpegthumbnailer # video thumbnails
    libgsf # odf thumbnails
    libraw # RAW thumbnails
    mate.engrampa # open archive files
    poppler # pdf thumbnails
    webp-pixbuf-loader # webp thumbnail
    xclip # to implement the "copy path" custom action
    xfce.xfconf # query xfce config
    bzip2
    gnutar
    gzip
    lz4
    unrar
    unzip
    xz
    zip
    zstd
  ];

  # Configure custom Thunar actions.
  home-manager.users.eudoxia.home.file = {
    ".config/Thunar/uca.xml".source = ./actions.xml;
  };
}
