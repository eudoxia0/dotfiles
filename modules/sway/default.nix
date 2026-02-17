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

  # Emoji picker using bemoji with ydotool backend (works with Chromium)
  emoji-picker = pkgs.writeShellScriptBin "emoji-picker" ''
    export BEMOJI_PICKER_CMD="${pkgs.wofi}/bin/wofi -d -i -p emoji"
    export BEMOJI_TYPE_CMD="ydotool type --"
    export BEMOJI_CLIP_CMD="${pkgs.wl-clipboard}/bin/wl-copy"
    exec ${pkgs.bemoji}/bin/bemoji -t
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
      wl-clipboard
      wlsunset
      swaylock
      bemoji
      emoji-picker
      take-screenshot
    ];
  };

  home-manager.users.eudoxia.home.file.".config/sway/config".source = ./sway.conf;
  home-manager.users.eudoxia.home.file.".config/swaylock/config".source = ./swaylock.conf;
}
