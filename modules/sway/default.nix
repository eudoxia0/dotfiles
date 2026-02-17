{
  config,
  pkgs,
  lib,
  ...
}:

let
  take-screenshot = pkgs.writeShellScriptBin "take-screenshot" ''
    grim -g "$(slurp)" ~/screenshot.png
  '';
  wofi-emoji-and-paste = pkgs.writeShellScriptBin "wofi-emoji-and-paste" ''
      wofi-emoji && wl-paste -t TEXT
  '';
in
{
  # Sway window manager configuration
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      mako # notification system developed by swaywm maintainer
      slurp
      grim
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
      wlsunset
      swaylock
      wofi-emoji
      take-screenshot
      wofi-emoji-and-paste
    ];
  };

  home-manager.users.eudoxia.home.file.".config/sway/config".source = ./sway.conf;
  home-manager.users.eudoxia.home.file.".config/swaylock/config".source = ./swaylock.conf;
}
