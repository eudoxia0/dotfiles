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
  users.users.eudoxia.extraGroups = [
    "scanner"
    "lp"
  ];

  # Add support for Brother scanners.
  hardware.sane.brscan5.enable = true;

  # Install GNOME Document Scanner.
  users.users.eudoxia.packages = with pkgs; [
    simple-scan
  ];
}
