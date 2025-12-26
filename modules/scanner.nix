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

  # Add support for Brother scanners.
  hardware.sane.brscan4.enable = true;

  # Install GNOME Document Scanner.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    simple-scan
  ];
}
