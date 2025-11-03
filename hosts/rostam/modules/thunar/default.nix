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
