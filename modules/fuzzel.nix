{
  config,
  pkgs,
  lib,
  ...
}:

let
  fuzzel-launcher = pkgs.writeShellScriptBin "fuzzel-launcher" ''
    choice=$(printf "1Password\nZed\nCalibre" | fuzzel --dmenu)
    case "$choice" in
      "1Password") 1password ;;
      "Zed") zed ;;
      "Calibre") calibre ;;
    esac
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    fuzzel
    fuzzel-launcher
  ];
}
