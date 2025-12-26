{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Enable SANE scanners.
  hardware.sane.enable = true;

  # Add myself to the scanner and printer groups.
  users.users.eudoxia.extraGroups = [ "scanner" "lp" ];
}
