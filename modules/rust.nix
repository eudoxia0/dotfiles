{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    cargo-deny
    cargo-machete
    cargo-watch
    rustup
  ];
}
