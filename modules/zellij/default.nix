{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home = {
    packages = [ pkgs.zellij ];
    file = {
      ".config/zellij/config.kdl".source = ./zellij.kdl;
    };
  };
}
