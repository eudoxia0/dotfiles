{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.emacs.d/early-init.el - - - - ${dotfilesDir}/modules/emacs/early-init.el"
    "L+ /home/eudoxia/.emacs.d/init.el - - - - ${dotfilesDir}/modules/emacs/init.el"
  ];

  environment.sessionVariables = {
    EDITOR = "emacs";
  };
}
