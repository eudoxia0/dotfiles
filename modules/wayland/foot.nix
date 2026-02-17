{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "monospace:size=12";
        shell = "nu";
      };
    };
  };
}
