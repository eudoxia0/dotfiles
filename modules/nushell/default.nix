{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  # Install nushell.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    nushell
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/nushell/config.nu - - - - ${dotfilesDir}/modules/nushell/config.nu"
  ];
}
