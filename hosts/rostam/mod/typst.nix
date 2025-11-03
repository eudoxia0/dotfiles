{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    typst
  ];
}
