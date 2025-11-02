{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia = {
    programs.bash.enable = true;
    home = {
      shellAliases = {
        cdt = "cd ~/dotfiles/hosts/rostam";
        fd = "fd -HI";
        find = "echo 'use fd instead'";
        ls = "ls -1 --color";
      };
    };
  };
}
