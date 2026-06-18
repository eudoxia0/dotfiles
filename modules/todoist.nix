{
  config,
  pkgs,
  lib,
  ...
}:

let
  todoist = pkgs.writeShellScriptBin "todoist" ''
    TZ=Australia/Sydney exec ${pkgs.todoist-electron}/bin/todoist-electron "$@"
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    todoist
  ];
}
