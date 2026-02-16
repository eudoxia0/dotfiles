{
  config,
  pkgs,
  lib,
  ...
}:

let
  fuzzel-launcher = pkgs.writeShellScriptBin "fuzzel-launcher" ''
    choice=$(printf "1Password\nCalibre\nDiet Tracker\nJournal\nLock Screen\nSound Settings\nTodoist\nZed" | fuzzel --dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Calibre") calibre ;;
      "Diet Tracker") chromium-browser --app=http://localhost:12001/ ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
      "Lock Screen") swaylock ;;
      "Sound Settings") pavucontrol ;;
      "Todoist") todoist-wayland ;;
      "Zed") zeditor ;;
    esac
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    fuzzel
    fuzzel-launcher
  ];
}
