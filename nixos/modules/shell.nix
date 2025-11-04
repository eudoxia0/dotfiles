{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    programs.bash.enable = true;
    home = {
      shellAliases = {
        cdt = "cd ~/dotfiles/nixos/";
        ls = "ls -1 --color";
      };
    };
  };
}
