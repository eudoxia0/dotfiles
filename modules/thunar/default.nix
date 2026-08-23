{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  programs.thunar = {
    enable = true;
    plugins = with pkgs; [
      thunar-archive-plugin
      thunar-volman
    ];
  };

  # mount etc.
  services.gvfs.enable = true;

  # thumbnails service
  services.tumbler.enable = true;

  # Configure custom Thunar actions and the bookmarks on the left pane.
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/Thunar/uca.xml - - - - ${dotfilesDir}/modules/thunar/actions.xml"
    "L+ /home/eudoxia/.config/gtk-3.0/bookmarks - - - - ${dotfilesDir}/modules/thunar/bookmarks.txt"
  ];

  # Other packages needed by Thunar.
  users.users.eudoxia.packages = with pkgs; [
    ffmpegthumbnailer # video thumbnails
    libgsf # odf thumbnails
    libraw # RAW thumbnails
    webp-pixbuf-loader # webp thumbnail

    xclip # to implement the "copy path" custom action
    xfconf # query xfce config

    bzip2
    gnutar
    gzip
    lz4
    engrampa # open archive files
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
