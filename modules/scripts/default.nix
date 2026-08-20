{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/bin/add-to-dictionary - - - - ${dotfilesDir}/modules/scripts/add-to-dictionary.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/timestamp - - - - ${dotfilesDir}/modules/scripts/timestamp.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/consolidate-images - - - - ${dotfilesDir}/modules/scripts/consolidate-images.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/find-syncthing-conflicts - - - - ${dotfilesDir}/modules/scripts/find-syncthing-conflicts.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/compress-pdf - - - - ${dotfilesDir}/modules/scripts/compress-pdf.sh"
    "L+ /home/eudoxia/.eudoxia.d/bin/count-inodes - - - - ${dotfilesDir}/modules/scripts/count-inodes.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/count-inodes-du - - - - ${dotfilesDir}/modules/scripts/count-inodes-du.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/backup - - - - ${dotfilesDir}/modules/scripts/backup.sh"
    "L+ /home/eudoxia/.eudoxia.d/bin/heic2jpg - - - - ${dotfilesDir}/modules/scripts/heic2jpg.sh"
    "L+ /home/eudoxia/.eudoxia.d/bin/punct - - - - ${dotfilesDir}/modules/scripts/punct.py"
    "L+ /home/eudoxia/.eudoxia.d/bin/get-cpu-governor - - - - ${dotfilesDir}/modules/scripts/get-cpu-governor.sh"
  ];
}
