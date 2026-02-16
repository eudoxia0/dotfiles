{
  config,
  pkgs,
  lib,
  ...
}:

let
  fuzzel-launcher = pkgs.writeShellScriptBin "fuzzel-launcher" ''
    choice=$(printf "1Password\nCalibre\nJournal\nTodoist\nZed" | fuzzel --dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Calibre") calibre ;;
      "Journal") chromium-browser --app=http://localhost:12003/ ;;
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
