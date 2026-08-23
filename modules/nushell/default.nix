{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  # Install nushell.
  users.users.eudoxia.packages = with pkgs; [
    nushell
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/nushell/config.nu - - - - ${dotfilesDir}/modules/nushell/config.nu"
  ];
}
