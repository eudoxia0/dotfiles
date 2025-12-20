{
  config,
  pkgs,
  lib,
  ...
}:

let
  headphones = "88:0E:85:B1:FD:8E";
  earbuds = "D8:19:7A:04:C8:E3";

  connect-headphones = pkgs.writeShellScriptBin "connect-headphones" ''
    bluetoothctl connect ${headphones}
  '';
  disconnect-headphones = pkgs.writeShellScriptBin "disconnect-headphones" ''
    bluetoothctl disconnect ${headphones}
  '';

  connect-earbuds = pkgs.writeShellScriptBin "connect-earbuds" ''
    bluetoothctl connect ${earbuds}
  '';
  disconnect-earbuds = pkgs.writeShellScriptBin "disconnect-earbuds" ''
    bluetoothctl disconnect ${earbuds}
  '';
in
{
  hardware.bluetooth.enable = true;

  home-manager.users.eudoxia.home.packages = [
    connect-headphones
    disconnect-headphones
    connect-earbuds
    disconnect-earbuds
  ];
}
