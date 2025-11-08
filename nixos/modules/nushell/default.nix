{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install nushell.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    nushell
  ];
}
