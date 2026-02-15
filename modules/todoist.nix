{
  config,
  pkgs,
  lib,
  ...
}:

let
  todoist-wayland = pkgs.writeShellScriptBin "todoist-wayland" ''
    TZ=Australia/Sydney exec ${pkgs.todoist-electron}/bin/todoist-electron --ozone-platform-hint=auto "$@"
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    todoist-electron
    todoist-wayland
  ];
}
