{
  config,
  pkgs,
  lib,
  inform7-nix,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.inform
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.inweb
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.intest
  ];
}
