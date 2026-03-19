{
  config,
  pkgs,
  lib,
  ...
}:

let
  rofi-launcher = pkgs.writeShellScriptBin "rofi-launcher" ''
    choice=$(printf "1Password\nBaobab\nCalibre\nChromium\nDiet Tracker\nFonts\nHeroic\nJournal\nLibreOffice\nLock\nMusic Player\nObsidian\nScreenshot\nSignal\nSound Settings\nTodoist\nTransmission\nZed" | rofi -i -dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Baobab") baobab ;;
      "Calibre") calibre ;;
      "Chromium") chromium-browser ;;
      "Diet Tracker") chromium-browser --app=http://localhost:12001/ ;;
      "Fonts") font-manager ;;
      "Heroic") heroic ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
      "LibreOffice") libreoffice ;;
      "Lock") xscreensaver-command -lock ;;
      "Music Player") strawberry ;;
      "Obsidian") obsidian ;;
      "Screenshot") scrot -f -s ;;
      "Signal") signal-desktop ;;
      "Sound Settings") pavucontrol ;;
      "Todoist") todoist-x11 ;;
      "Transmission") transmission-gtk ;;
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
