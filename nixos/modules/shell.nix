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
        cdt = "cd ~/dotfiles/hosts/rostam";
        ls = "ls -1 --color";
      };
    };
  };
}
