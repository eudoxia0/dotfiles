{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    guile
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.guile - - - - ${dotfilesDir}/modules/guile/init.scm"
    "L+ /home/eudoxia/.guile-modules/eudoxia - - - - ${dotfilesDir}/modules/guile/eudoxia"
  ];

  home-manager.users.eudoxia.home.sessionVariables = {
    GUILE_LOAD_PATH = "/home/eudoxia/.guile-modules";
  };
}
