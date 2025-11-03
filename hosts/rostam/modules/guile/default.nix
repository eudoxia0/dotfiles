{
  config,
  pkgs,
  lib,
  ...
}:

{

  home-manager.users.eudoxia.home.packages = with pkgs; [
    guile
  ];

  home-manager.users.eudoxia.home.file.".guile".source = ./init.scm;
}
