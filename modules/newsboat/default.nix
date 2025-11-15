{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    newsboat
  ];
  home-manager.users.eudoxia.home.file = {
    ".newsboat/urls".source = ./urls.txt;
    ".newsboat/config".source = ./config.txt;
  };
}
