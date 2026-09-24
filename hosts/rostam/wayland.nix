{
  config,
  pkgs,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/sway/extra/rostam - - - - ${dotfilesDir}/hosts/rostam/sway.conf"
  ];
}
