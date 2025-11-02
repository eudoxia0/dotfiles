{
  config,
  pkgs,
  lib,
  ...
}:

{

  home-manager.users.eudoxia.home.file.".local/share/eudoxia/panther.jpg".source = ./panther.jpg;
}
