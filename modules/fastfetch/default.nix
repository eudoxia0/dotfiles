{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.fastfetch ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/data/nerv/nerv-color.txt - - - - ${dotfilesDir}/modules/fastfetch/nerv/nerv-color.txt"
    "L+ /home/eudoxia/.eudoxia.d/bin/nervfetch - - - - ${dotfilesDir}/modules/fastfetch/nerv/nervfetch.sh"
  ];
}
