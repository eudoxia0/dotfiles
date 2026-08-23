{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = with pkgs; [
    guile
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.guile - - - - ${dotfilesDir}/modules/guile/init.scm"
    "L+ /home/eudoxia/.guile-modules/eudoxia - - - - ${dotfilesDir}/modules/guile/eudoxia"
  ];

  environment.sessionVariables = {
    GUILE_LOAD_PATH = "/home/eudoxia/.guile-modules";
  };
}
