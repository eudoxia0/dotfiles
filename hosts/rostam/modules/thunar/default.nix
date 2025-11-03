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
    poppler # pdf thumbnails
    libraw # RAW thumbnails
    webp-pixbuf-loader # webp thumbnail
    xfce.xfconf # query xfce config
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/Thunar/uca.xml".source = ./actions.xml;
  };
}
