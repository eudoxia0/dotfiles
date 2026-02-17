{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Sway window manager configuration
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  home-manager.users.eudoxia.home.file = {
    ".config/sway/config".source = ./sway.conf;
  };
}
