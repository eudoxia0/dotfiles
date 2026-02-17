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

  emoji-picker = pkgs.writeShellScriptBin "emoji-picker" ''
    rofimoji --action copy
    ydotool key 29:1 47:1 47:0 29:0
  '';
in
{
  # Sway window manager configuration
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      mako
      slurp
      grim
      rofimoji
      wl-clipboard
      wlsunset
      swaylock
      emoji-picker
      take-screenshot
    ];
  };

  home-manager.users.eudoxia.home.file.".config/sway/config".source = ./sway.conf;
  home-manager.users.eudoxia.home.file.".config/swaylock/config".source = ./swaylock.conf;
}
