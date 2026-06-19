{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
  };

  # mount etc.
  services.gvfs.enable = true;

  # thumbnails service
  services.tumbler.enable = true;

  # Configure custom Thunar actions.
  home-manager.users.eudoxia.home.file = {
    ".config/Thunar/uca.xml".source = ./actions.xml;
  };

  # Configure the bookmarks on the left pane.
  home-manager.users.eudoxia.home.file = {
    ".config/gtk-3.0/bookmarks".source = ./bookmarks.txt;
  };

  # Other packages needed by Thunar.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    ffmpegthumbnailer # video thumbnails
    libgsf # odf thumbnails
    libraw # RAW thumbnails
    poppler # pdf thumbnails
    webp-pixbuf-loader # webp thumbnail

    xclip # to implement the "copy path" custom action
    xfce.xfconf # query xfce config

    bzip2
    gnutar
    gzip
    lz4
    mate.engrampa # open archive files
    unrar
    unzip
    xz
    zip
    zstd
  ];

  # xfconf
  home-manager.users.eudoxia.xfconf.settings = {
    thunar = {
      "hidden-bookmarks" = [
        "file:///home/eudoxia/Desktop"
        "recent:///"
        "computer:///"
      ];
    };
  };
}
