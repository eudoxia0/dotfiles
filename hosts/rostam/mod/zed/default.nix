{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    zed-editor
  ];
  home-manager.users.eudoxia.home.file = {
    ".config/zed/settings.json".source = ./settings.json;
  };
}
