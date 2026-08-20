{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.XCompose - - - - ${dotfilesDir}/modules/x11/xcompose/xcompose.xcm"
  ];
}
