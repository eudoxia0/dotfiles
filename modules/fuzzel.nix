{
  config,
  pkgs,
  lib,
  ...
}:

let
  fuzzel-launcher = pkgs.writeShellScriptBin "fuzzel-launcher" ''
    choice=$(printf "1Password\nCalibre\nChromium\nDiet Tracker\nJournal\nLock Screen\nScreenshot\nSignal\nSound Settings\nTodoist\nZed" | fuzzel --dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Calibre") calibre ;;
      "Chromium") chromium-browser ;;
      "Diet Tracker") chromium-browser --app=http://localhost:12001/ ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
      "Lock Screen") swaylock ;;
      "Screenshot") take-screenshot ;;
      "Signal") signal-desktop --ozone-platform-hint=auto ;;
      "Sound Settings") pavucontrol ;;
      "Todoist") todoist-wayland ;;
      "Zed") WAYLAND_DISPLAY= zeditor ;;
    esac
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    fuzzel
    fuzzel-launcher
  ];
}
