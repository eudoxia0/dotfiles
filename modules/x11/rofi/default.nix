{
  config,
  pkgs,
  lib,
  ...
}:

let
  rofi-launcher = pkgs.writeShellScriptBin "rofi-launcher" ''
    choice=$(printf "1Password\nCalibre\nChromium\nDiet Tracker\nJournal\nLock Screen\nScreenshot\nSignal\nSound Settings\nTodoist\nZed" | rofi -dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Calibre") calibre ;;
      "Chromium") chromium-browser ;;
      "Diet Tracker") chromium-browser --app=http://localhost:12001/ ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
      "Lock Screen") xscreensaver-command -lock ;;
      "Screenshot") scrot -f -s ;;
      "Signal") signal-desktop ;;
      "Sound Settings") pavucontrol ;;
      "Todoist") todoist-x11 ;;
      "Zed") zeditor ;;
    esac
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    rofi
    rofi-launcher
  ];
}
