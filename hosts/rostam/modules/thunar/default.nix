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
}
