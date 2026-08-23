{
  config,
  pkgs,
  lib,
  ...
}:

{
  users.users.eudoxia.packages = [ pkgs.gargoyle ];
}
