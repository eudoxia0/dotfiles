{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    beets
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/beets/config.yaml".source = ./beets.yaml;
  };
}
