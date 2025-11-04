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
  home-manager.users.eudoxia.home.file.".guile-modules/eudoxia" = {
    source = ./eudoxia;
    recursive = true;
  };

  home-manager.users.eudoxia.home.sessionVariables = {
    GUILE_LOAD_PATH = "/home/eudoxia/.guile-modules";
  };
}
