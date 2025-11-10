{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    programs.bash.enable = true;
  };

  home-manager.users.eudoxia.home = {
    sessionPath = [
      "$HOME/.eudoxia.d/bin"
    ];
  };
}
