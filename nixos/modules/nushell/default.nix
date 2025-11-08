{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install nushell.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    nushell
  ];

  # Copy nushell config.
  home-manager.users.eudoxia.home.file.".config/nushell/config.nu".source = ./config.nu;
}
