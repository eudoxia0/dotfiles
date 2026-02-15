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
    extraPackages = with pkgs; [
      mako # notification system developed by swaywm maintainer
      grim # screenshot functionality
      slurp # screenshot functionality
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
      wmenu
      wlsunset
    ];
  };

  home-manager.users.eudoxia.home.file.".config/sway/config".source = ./sway.conf;
}
