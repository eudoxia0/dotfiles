{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".XCompose".source = ./xcompose.xcm;
  };
}
