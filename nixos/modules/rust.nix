{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    rustup
  ];

  home-manager.users.eudoxia.home.shellAliases = {
    cf = "cargo +nightly fmt";
    ck = "cargo check";
    cl = "cargo clippy --all-targets -- -D warnings";
  };
}
