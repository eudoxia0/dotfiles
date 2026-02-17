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
      slurp
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
      wmenu
      wlsunset
      swaylock
      wofi-emoji
    ];
  };

  home-manager.users.eudoxia.home.file.".config/sway/config".source = ./sway.conf;
  home-manager.users.eudoxia.home.file.".config/swaylock/config".source = ./swaylock.conf;
}
