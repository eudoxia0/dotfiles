{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.git ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/git/config - - - - ${dotfilesDir}/modules/git/gitconfig.txt"
  ];
}
