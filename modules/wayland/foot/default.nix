{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "monospace:size=12";
        shell = "nu";
      };
    };
  };
}
