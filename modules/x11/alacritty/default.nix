{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.alacritty ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/alacritty/alacritty.toml - - - - ${dotfilesDir}/modules/x11/alacritty/config.toml"
  ];
}
