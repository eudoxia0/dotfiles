{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    cargo
    rustc
  ];

  home-manager.users.eudoxia.home.shellAliases = {
    cf = "cargo +nightly fmt";
    ck = "cargo check";
    cl = "cargo clippy --all-targets -- -D warnings";
  };
}
