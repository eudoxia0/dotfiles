{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia = {
    programs.bash.enable = true;
    home = {
      shellAliases = {
        cdt = "cd ~/dotfiles/hosts/rostam";
        cf = "cargo +nightly fmt";
        ck = "cargo check";
        cl= "cargo clippy --all-targets -- -D warnings";
        fd = "fd -HI";
        find = "echo 'use fd instead'";
        gb = "git branch";
        gcam = "git commit -a -m";
        gco = "git checkout";
        gd = "git pull origin";
        gs = "git status";
        gu = "git push -u origin HEAD";
        ls = "ls -1 --color";
      };
    };
  };
}
