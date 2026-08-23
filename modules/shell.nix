{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.bash.enable = true;

  home-manager.users.eudoxia.home = {
    sessionPath = [
      "$HOME/.eudoxia.d/bin"
      "$HOME/.cargo/bin"
    ];
  };
}
