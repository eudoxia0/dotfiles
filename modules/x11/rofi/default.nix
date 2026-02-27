{
  config,
  pkgs,
  lib,
  ...
}:

let
  rofi-launcher = pkgs.writeShellScriptBin "rofi-launcher" ''
    choice=$(printf "1Password\nCalibre\nChromium\nDiet Tracker\nJournal\nLock\nObsidian\nScreenshot\nSignal\nSound Settings\nTodoist\nZed" | rofi -i -dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Calibre") calibre ;;
      "Chromium") chromium-browser ;;
      "Diet Tracker") chromium-browser --app=http://localhost:12001/ ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
      "Lock") xscreensaver-command -lock ;;
      "Obsidian") obsidian ;;
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
